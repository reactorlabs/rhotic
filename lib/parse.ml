open Angstrom
open Expr

exception Parse_error of string

(* Reserved keywords/literals *)
let reserved =
  [ "NA"
  ; "F"
  ; "T"
  ; "NA_integer_"
  ; "NA_character_"
  ; "data.frame"
  ; "as.logical"
  ; "as.integer"
  ; "as.character"
  ; "function"
  ; "if"
  ; "else"
  ; "for"
  ]

(* Common helpers for parsers *)
let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
let is_eol = function
  | '\n' | '\r' -> true
  | _ -> false
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
let is_quote = function
  | '"' -> true
  | _ -> false

let is_blank x = is_space x || is_eol x
let is_first_ident x = '.' == x || is_letter x
let is_ident x = '_' == x || is_first_ident x || is_digit x

let ws = take_while is_space
let blank = take_while is_blank
let with_ws p = ws *> p <* ws
let with_blank p = blank *> p <* blank

let parens p = char '(' *> with_blank p <* char ')'
let bracks1 p = char '[' *> with_blank p <* char ']'
let bracks2 p = string "[[" *> with_blank p <* string "]]"
let braces p = blank *> char '{' *> with_blank p <* char '}'
let quotes p = char '"' *> p <* char '"'

let parens_comma_sep p = parens @@ sep_by (char ',') (with_blank p)

(* Program parser *)
let program =
  (* literal ::= boolean | integer | string
     boolean ::= 'NA | 'F' | 'T'
     integer ::= 'NA_integer_' | [0-9]+
     string  ::= 'NA_character_' | '"' [any non-quote character]* '"'

     NOTE: Order is significant! Need to attempt parsing "NA_integer_" and "NA_character_" before
     "NA". Otherwise NA_integer_ (and NA_character_) will be accepted as NA and _integer_
     (and _character_) will be rejected. *)
  let literal =
    let boolean =
      string "NA" *> return NA_bool
      <|> string "F" *> return (Bool false)
      <|> string "T" *> return (Bool true) in
    let integer =
      string "NA_integer_" *> return NA_int
      <|> (take_while1 is_digit >>| fun i -> Int (int_of_string i)) in
    let negative = char '-' *> (take_while1 is_digit >>| fun i -> Int ~-(int_of_string i)) in
    let str =
      string "NA_character_" *> return NA_str <|> (quotes (take_till is_quote) >>| fun s -> Str s)
    in
    str <|> integer <|> negative <|> boolean >>| fun l -> Lit l in

  (* An identifier
      - starts with a letter or a .
      - uses alphanumeric characters or . or _
      - is not a reserved word *)
  let identifier =
    let symbol c = if is_first_ident c then take_while is_ident else fail "ident" in
    let not_keyword s = if List.mem s reserved then fail "keyword" else return s in
    peek_char_fail >>= symbol >>= not_keyword in

  let variable = identifier >>| fun s -> Var s in

  (* simple_expr ::= literal | variable *)
  let simple_expr = literal <|> variable in

  (* This can be used as a subset expression (base = simple_exr), or a subset
     assignment statement (base = variable).

     Also, base$name is a shorthand for base[["name"]].

     subset_base ::= base  '['              ']'
                   | base  '[' simple_expr  ']'
                   | base '[[' simple_expr ']]'
                   | base  '$' identifier *)
  let subset base =
    base <* ws >>= fun be ->
    peek_char >>= function
    | Some '[' ->
        char '[' *> blank *> char ']' *> return (Subset1 (be, None))
        <|> (bracks1 simple_expr >>| fun e -> Subset1 (be, Some e))
        <|> (bracks2 simple_expr >>| fun e -> Subset2 (be, e))
    | Some '$' -> char '$' *> ws *> identifier >>| fun name -> Subset2 (be, Lit (Str name))
    | _ -> fail "not subsetting" in

  let expr =
    (* combine ::= 'c '(' simple_expr ',' ... ',' simple_expr ')' *)
    let combine = char 'c' *> ws *> parens_comma_sep simple_expr >>| fun es -> Combine es in

    (* dataframe ::= 'data.frame' '( binding ',' ... ',' binding ')
       binding   ::= identifier '=' simple_expr *)
    let dataframe =
      let binding =
        lift2 (fun id se -> (id, se)) (identifier <* with_blank (char '=')) simple_expr in
      string "data.frame" *> ws *> parens_comma_sep binding >>| fun bs -> Dataframe_Ctor bs in

    (* coerce_op ::= c_op simple_expr
       c_op      ::= 'as.logical' | 'as.integer' | 'as.character' *)
    let coerce_op =
      let c_op =
        string "as.logical" *> ws *> return T_Bool
        <|> string "as.integer" *> ws *> return T_Int
        <|> string "as.character" *> ws *> return T_Str in
      lift2 (fun op se -> Coerce_Op (op, se)) c_op (parens simple_expr) in

    (* unary_op ::= u_op simple_Expr
       u_op     ::= '!' | '+' | '-' *)
    let unary_op =
      let u_op =
        char '!' *> ws *> return Logical_Not
        <|> char '+' *> ws *> return Unary_Plus
        <|> char '-' *> ws *> return Unary_Minus in
      lift2 (fun op se -> Unary_Op (op, se)) u_op simple_expr in

    (* binary_op ::= simple_expr b_op simple_expr
       b_op      ::=  '+' |  '-' | '*' | '/' | '%'
                   | '<=' | '>=' | '<' | '>'
                   | '==' | '!='
                   | '&&' | '||'

       NOTE: Order is significant! Need to attempt parsing "<=" and ">=" before "<" and ">". Otherwise
       "<" (or ">") is parsed and accepted, leaving "=" in the input. Similarly, "&&" and "||" need
       to be parsed before "&" and "|". *)
    let binary_op =
      let b_op =
        with_ws (char '+') *> return (Arithmetic Plus)
        <|> with_ws (char '-') *> return (Arithmetic Minus)
        <|> with_ws (char '*') *> return (Arithmetic Times)
        <|> with_ws (char '/') *> return (Arithmetic Int_Divide)
        <|> with_ws (char '%') *> return (Arithmetic Modulo)
        <|> with_ws (string "<=") *> return (Relational Less_Equal)
        <|> with_ws (string ">=") *> return (Relational Greater_Equal)
        <|> with_ws (char '<') *> return (Relational Less)
        <|> with_ws (char '>') *> return (Relational Greater)
        <|> with_ws (string "==") *> return (Relational Equal)
        <|> with_ws (string "!=") *> return (Relational Not_Equal)
        <|> with_ws (string "&&") *> return (Logical And)
        <|> with_ws (string "||") *> return (Logical Or)
        <|> with_ws (char '&') *> return (Logical Elementwise_And)
        <|> with_ws (char '|') *> return (Logical Elementwise_Or)
        <|> with_ws (char ':') *> return Seq in
      lift3 (fun se1 op se2 -> Binary_Op (op, se1, se2)) simple_expr b_op simple_expr in

    (* subset  ::= subset1 | subset2
       subset1 ::= simple_expr  '['             ']'
                 | simple_expr  '[' simple_expr ']'
       subset2 ::= simple_expr '[[' simple_expr ']]'
                 | identifier   '$' identifier *)
    let subset' = subset simple_expr in

    (* call ::= identifier '(' simple_expr ',' ... ',' simple_expr ')' *)
    let call = lift2 (fun f a -> Call (f, a)) (identifier <* ws) (parens_comma_sep simple_expr) in

    let se = simple_expr >>| fun se -> Simple_Expression se in

    (* expression ::= combine | dataframe
                       | coerce_op | binary_op | unary_op
                       | subset | call | simple_expr
       NOTE: Order is significant, want to parse binary ops before unary ops, so that -4 is parsed
       as a literal rather than a unary op. *)
    combine <|> dataframe <|> coerce_op <|> binary_op <|> unary_op <|> subset' <|> call <|> se in

  (* stmt_list ::= stmt sep ... sep stmt
     sep       ::= ; | [\n\r]+
     block     ::= '{' stmt_list '}' *)
  let stmt_list stmt =
    let trailing = with_blank (char ';') <|> blank *> return '\n' in
    let seq_sep = with_blank (char ';') <|> take_while1 is_eol *> return '\n' in
    sep_by seq_sep (with_ws stmt) <* trailing in

  let arrow = with_ws (string "<-") in
  let block stmt = braces (stmt_list stmt) in

  let stmt_no_fun =
    fix (fun stmt ->
        let block = block stmt in

        (* assign ::= identifier '<-' expr *)
        let assign = lift2 (fun v e -> Assign (v, e)) (identifier <* arrow) expr in

        (* subset_assign  ::= subset1_assign | subset2_assign
           subset1_assign ::= identifier  '['              ']' '<-' expr
                             | identifier  '[' simple_expr  ']' '<-' expr
           subset2_assign ::= identifier '[[' simple_expr ']]' '<-' expr
                             | identifier  '$' identifier       '<-' expr *)
        let subset_assign =
          let[@warning "-4-8"] subset_assign' lhs rhs =
            match lhs with
            | Subset1 (Var x, se) -> Subset1_Assign (x, se, rhs)
            | Subset2 (Var x, se) -> Subset2_Assign (x, se, rhs) in
          lift2 subset_assign' (subset variable <* arrow) expr in

        (* if ::= if ( simple_expr ) { stmt_list }
                | if ( simple_expr ) { stmt_list } else { stmt_list } *)
        let if_stmt =
          lift3
            (fun se s1 s2 -> If (se, s1, s2))
            (string "if" *> ws *> parens simple_expr)
            block
            (with_ws (string "else") *> block)
          <|> lift2 (fun se s -> If (se, s, [])) (string "if" *> ws *> parens simple_expr) block
        in

        (* for ::= 'for '(' id 'in' simple_expr ')' block *)
        let for_loop =
          let loop_expr =
            lift2 (fun id se -> (id, se)) (identifier <* with_ws (string "in")) simple_expr in
          lift2
            (fun (id, se) stmts -> For (id, se, stmts))
            (string "for" *> ws *> parens loop_expr)
            block in

        let e = expr >>| fun e -> Expression e in

        (* statement_no_fun ::= assign | subset_assign | if_stmt | for_loop | expr

           Function definitions are only allowed at the top level, otherwise parser rejects. *)
        assign <|> subset_assign <|> if_stmt <|> for_loop <|> e) in

  (* function_def ::= id <- function ( id , ... , id ) block

     The block in a function definition may not contain function definition statements. *)
  let fun_def =
    lift3
      (fun f params stmts -> Function_Def (f, params, stmts))
      (identifier <* arrow <* string "function")
      (with_ws @@ parens_comma_sep identifier)
      (block stmt_no_fun) in

  (* program ::= stmt_list

     All statements (including function definitions) are allowed at the top level.

     NOTE: Order is significant! Need to attempt parsing a function definition first. *)
  stmt_list (fun_def <|> stmt_no_fun)

let parse (str : string) =
  match parse_string ~consume:All program str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)

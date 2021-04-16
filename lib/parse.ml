open Angstrom
open Expr

exception Parse_error of string

(* Reserved keywords/literals *)
let reserved =
  [ "NA_b"
  ; "F"
  ; "T"
  ; "NA_i"
  ; "NA_s"
  ; "combine"
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
     boolean ::= 'NA_b' | 'F' | 'T'
     integer ::= 'NA_i' | [0-9]+
     string  ::= 'NA_s' | '"' [any non-quote character]* '"' *)
  let literal =
    let boolean =
      string "NA_b" *> return NA_bool
      <|> string "F" *> return (Bool false)
      <|> string "T" *> return (Bool true) in
    let integer =
      string "NA_i" *> return NA_int <|> (take_while1 is_digit >>| fun i -> Int (int_of_string i))
    in
    let str = string "NA_s" *> return NA_str <|> (quotes (take_till is_quote) >>| fun s -> Str s) in
    boolean <|> integer <|> str >>| fun l -> Lit l in

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
    (* combine ::= 'combine' '(' simple_expr ',' ... ',' simple_expr ')' *)
    let combine = string "combine" *> ws *> parens_comma_sep simple_expr >>| fun es -> Combine es in

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
        string "as.logical" *> ws *> return To_Bool
        <|> string "as.integer" *> ws *> return To_Int
        <|> string "as.character" *> ws *> return To_Str in
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
       "<" (or ">") is parsed and accepted, leaving "=" in the input. *)
    let binary_op =
      let b_op =
        with_ws (char '+') *> return Plus
        <|> with_ws (char '-') *> return Minus
        <|> with_ws (char '*') *> return Times
        <|> with_ws (char '/') *> return Int_Divide
        <|> with_ws (char '%') *> return Modulo
        <|> with_ws (string "<=") *> return Less_Equal
        <|> with_ws (string ">=") *> return Greater_Equal
        <|> with_ws (char '<') *> return Less
        <|> with_ws (char '>') *> return Greater
        <|> with_ws (string "==") *> return Equal
        <|> with_ws (string "!=") *> return Not_Equal
        <|> with_ws (string "&&") *> return Logical_And
        <|> with_ws (string "||") *> return Logical_Or in
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
                    | coerce_op | unary_op | binary_op
                    | subset | call | simple_expr *)
    combine <|> dataframe <|> coerce_op <|> unary_op <|> binary_op <|> subset' <|> call <|> se in

  (* stmt_list ::= stmt sep ... sep stmt
      sep       ::= ; | [\n\r]+
      block     ::= '{' stmt_list '}' *)
  let stmt_list stmt =
    let trailing = with_blank (char ';') <|> blank *> return '\n' in
    let seq_sep = with_blank (char ';') <|> take_while1 is_eol *> return '\n' in
    sep_by seq_sep (with_ws stmt) <* trailing in

  let stmt =
    fix (fun stmt ->
        let arrow = with_ws (string "<-") in
        let block = braces (stmt_list stmt) in

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

        (* function_def ::= id <- function ( id , ... , id ) block *)
        let fun_def =
          lift3
            (fun f params stmts -> Function_Def (f, params, stmts))
            (identifier <* arrow <* string "function")
            (with_ws @@ parens_comma_sep identifier)
            block in

        (* if ::= if ( expr ) { stmt_list }
                | if ( expr ) { stmt_list } else { stmt_list } *)
        let if_stmt =
          lift3
            (fun e s1 s2 -> If (e, s1, s2))
            (string "if" *> ws *> parens expr)
            block
            (with_ws (string "else") *> block)
          <|> lift2 (fun e s -> If (e, s, [])) (string "if" *> ws *> parens expr) block in

        (* for ::= 'for '(' id 'in' expr ')' block *)
        let for_loop =
          let loop_expr = lift2 (fun id e -> (id, e)) (identifier <* with_ws (string "in")) expr in
          lift2
            (fun (id, e) stmts -> For (id, e, stmts))
            (string "for" *> ws *> parens loop_expr)
            block in

        let e = expr >>| fun e -> Expression e in

        (* statement ::= assign | subset_assign | fun_def | if_stmt | for_loop | expr *)
        assign <|> subset_assign <|> fun_def <|> if_stmt <|> for_loop <|> e) in

  (* program ::= stmt_list *)
  stmt_list stmt

let parse (str : string) =
  match parse_string ~consume:All program str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)

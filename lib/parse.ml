open Angstrom
open Expr

exception Parse_error of string

(* Reserved keywords/literals *)
let reserved = [ "NA_b"; "F"; "T"; "NA_i"; "NA_s"; "to_bool"; "to_int"; "to_str"; "combine" ]

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
let with_blank_ws p = blank *> p <* ws

let parens p = char '(' *> with_blank p <* char ')'
let bracks1 p = char '[' *> with_blank p <* char ']'
let bracks2 p = string "[[" *> with_blank p <* string "]]"
let braces p = blank *> char '{' *> p <* char '}'
let quotes p = char '"' *> p <* char '"'

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
  boolean <|> integer <|> str >>| fun l -> Lit l

(* An identifier
    - starts with a letter or a .
    - uses alphanumeric characters or . or _
    - is not a reserved word *)
let variable =
  let identifier =
    peek_char_fail >>= fun c -> if is_first_ident c then take_while is_ident else fail "ident" in
  identifier >>= fun s ->
  if List.mem s reserved then fail "keyword" else return s >>| fun s -> Var s

(* simple_expr ::= literal | variable *)
let simple_expr = literal <|> variable

let expr =
  (* combine ::= 'combine' '(' simple_expr ',' ... ',' simple_expr ')' *)
  let combine =
    string "combine" *> ws *> parens (sep_by (char ',') (with_blank simple_expr)) >>| fun es ->
    Combine es in

  (* unary_op    ::= op_expr | coerce_expr
     op_expr     ::= uop simple_expr
     u_op        ::= '!' | '+' | '-'
     coerce_expr ::= coerce ( simple_expr )
     coerce      ::= 'to_bool' | 'to_int' | 'to_str' *)
  let unary_op =
    let u_op =
      char '!' *> ws *> return Logical_Not
      <|> char '+' *> ws *> return Unary_Plus
      <|> char '-' *> ws *> return Unary_Minus in
    let coerce =
      string "to_bool" *> ws *> return To_Bool
      <|> string "to_int" *> ws *> return To_Int
      <|> string "to_str" *> ws *> return To_Str in
    let unop' op se = Unary_Op (op, se) in
    let op_expr = lift2 unop' u_op simple_expr in
    let coerce_expr = lift2 unop' coerce (parens simple_expr) in
    op_expr <|> coerce_expr in

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
    let binop' se1 op se2 = Binary_Op (op, se1, se2) in
    lift3 binop' simple_expr b_op simple_expr in

  (* subset  ::= subset1 | subset2
     subset1 ::= simple_expr  '['             ']'
               | simple_expr  '[' simple_expr ']'
     subset2 ::= simple_expr '[[' simple_expr ']]'
  *)
  let subset =
    simple_expr <* ws >>= fun be ->
    char '[' *> blank *> char ']' *> return (Subset1 (be, None))
    <|> (bracks1 simple_expr >>| fun e -> Subset1 (be, Some e))
    <|> (bracks2 simple_expr >>| fun e -> Subset2 (be, e)) in

  (* call ::= identifier '(' simple_expr ',' ... ',' simple_expr ')' *)
  let call =
    let[@warning "-4-8"] call' fn ses =
      match fn with
      | Var s -> Call (s, ses) in
    lift2 call' variable (ws *> parens (sep_by (char ',') (with_blank simple_expr))) in

  let se = simple_expr >>| fun se -> Simple_Expression se in

  (* expression ::= combine | unary_op | binary_op | subset | call | simple_expr *)
  combine <|> unary_op <|> binary_op <|> subset <|> call <|> se

(* TODO: statements and statement list *)

let try_parse p (str : string) =
  match parse_string ~consume:All p str with
  | Ok v -> v
  | Error msg -> raise (Parse_error msg)

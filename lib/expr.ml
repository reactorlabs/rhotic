type literal =
  | NA_bool
  | Bool    of bool
  | NA_int
  | Int     of int
  | NA_str
  | Str     of string
[@@deriving eq]

let show_lit = function
  | Bool b -> if b then "T" else "F"
  | Int i -> Int.to_string i
  | Str s -> s
  | NA_bool | NA_int | NA_str -> "NA"

module Identifier = struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let pp = Format.pp_print_text
end

type identifier = Identifier.t [@@deriving eq, show]

type simple_expression =
  | Lit of literal [@printer fun fmt l -> fprintf fmt "%s" (show_lit l)]
  | Var of identifier [@printer fun fmt -> fprintf fmt "%s"]
[@@deriving eq, show { with_path = false }]

type unary_op =
  | LogicalNot
  | Plus
  | Minus
  | To_Bool
  | To_Int
  | To_Str
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Plus
  | Minus
  | Times
  | IntDivide
  | Modulo
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NotEqual
  | And
  | Or
[@@deriving eq, show { with_path = false }]

type expression =
  | Combine of simple_expression list
  | UnOp    of unary_op * simple_expression
  | BinOp   of binary_op * simple_expression * simple_expression
  | Subset1 of simple_expression * simple_expression option
  | Subset2 of simple_expression * simple_expression
  | Call    of identifier * simple_expression list
[@@deriving eq, show { with_path = false }]

type statement =
  | FunctionDef    of identifier * identifier list * statement list
  | Assign         of identifier * expression
  | Subset1_Assign of identifier * expression
  | Subset2_Assign of identifier * expression
  | Expression     of expression
  | If             of expression * statement list * statement list option
  | For            of identifier * expression * statement list
[@@deriving eq, show { with_path = false }]

type type_tag =
  | T_Bool
  | T_Int
  | T_Str
[@@deriving eq]

let show_type = function
  | T_Bool -> "Bool"
  | T_Int -> "Int"
  | T_Str -> "Str"

type value = Vector of literal array * type_tag [@@deriving eq]

let show_val = function
  | Vector (a, t) ->
      let inner = a |> Array.map show_lit |> Array.to_list |> String.concat " " in
      "[" ^ inner ^ "]," ^ show_type t

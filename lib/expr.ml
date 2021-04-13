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
  | Logical_Not
  | Unary_Plus
  | Unary_Minus
  | To_Bool
  | To_Int
  | To_Str
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Plus
  | Minus
  | Times
  | Int_Divide
  | Modulo
  | Less
  | Less_Equal
  | Greater
  | Greater_Equal
  | Equal
  | Not_Equal
  | Logical_And
  | Logical_Or
[@@deriving eq, show { with_path = false }]

type expression =
  | Combine           of simple_expression list
  | Unary_Op          of unary_op * simple_expression
  | Binary_Op         of binary_op * simple_expression * simple_expression
  | Subset1           of simple_expression * simple_expression option
  | Subset2           of simple_expression * simple_expression
  | Call              of identifier * simple_expression list
  | Simple_Expression of simple_expression
[@@deriving eq, show { with_path = false }]

type statement =
  | Assign         of identifier * expression
  | Subset1_Assign of identifier * expression
  | Subset2_Assign of identifier * expression
  | Function_Def   of identifier * identifier list * statement list
  | If             of expression * statement list * statement list option
  | For            of identifier * expression * statement list
  | Expression     of expression
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

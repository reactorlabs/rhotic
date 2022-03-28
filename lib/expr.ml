open Containers

type literal =
  | NA_bool [@printer fun fmt _ -> fprintf fmt "NA"]
  | Bool    of bool [@printer fun fmt b -> fprintf fmt (if b then "T" else "F")]
  | NA_int [@printer fun fmt _ -> fprintf fmt "NA_integer_"]
  | Int     of int [@printer fun fmt -> fprintf fmt "%d"]
  | NA_str [@printer fun fmt _ -> fprintf fmt "NA_character_"]
  | Str     of string [@printer fun fmt -> fprintf fmt "\"%s\""]
[@@deriving eq, show { with_path = false }]

type type_tag =
  | T_Bool [@printer fun fmt _ -> fprintf fmt "Bool"]
  | T_Int [@printer fun fmt _ -> fprintf fmt "Int"]
  | T_Str [@printer fun fmt _ -> fprintf fmt "Str"]
[@@deriving eq, ord, show { with_path = false }]

module Identifier = struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let pp = Format.pp_print_text
  let prefix ?(pre = "") s = pre ^ "%" ^ s
end

type identifier = Identifier.t [@@deriving eq, ord, show]

module Env = Map.Make (Identifier)

type unary_op =
  | Logical_Not [@printer fun fmt _ -> fprintf fmt "!"]
  | Unary_Plus [@printer fun fmt _ -> fprintf fmt "+"]
  | Unary_Minus [@printer fun fmt _ -> fprintf fmt "-"]
  | As_Logical [@printer fun fmt _ -> fprintf fmt "as.logical"]
  | As_Integer [@printer fun fmt _ -> fprintf fmt "as.integer"]
  | As_Character [@printer fun fmt _ -> fprintf fmt "as.character"]
  | Is_Logical [@printer fun fmt _ -> fprintf fmt "is.logical"]
  | Is_Integer [@printer fun fmt _ -> fprintf fmt "is.integer"]
  | Is_Character [@printer fun fmt _ -> fprintf fmt "is.character"]
  | Is_NA [@printer fun fmt _ -> fprintf fmt "is.na"]
  | Length [@printer fun fmt _ -> fprintf fmt "length"]

and arithmetic_op =
  | Plus [@printer fun fmt _ -> fprintf fmt "+"]
  | Minus [@printer fun fmt _ -> fprintf fmt "-"]
  | Times [@printer fun fmt _ -> fprintf fmt "*"]
  | Int_Divide [@printer fun fmt _ -> fprintf fmt "/"]
  | Modulo [@printer fun fmt _ -> fprintf fmt "%%"]

and relational_op =
  | Less [@printer fun fmt _ -> fprintf fmt "<"]
  | Less_Equal [@printer fun fmt _ -> fprintf fmt "<="]
  | Greater [@printer fun fmt _ -> fprintf fmt ">"]
  | Greater_Equal [@printer fun fmt _ -> fprintf fmt ">="]
  | Equal [@printer fun fmt _ -> fprintf fmt "=="]
  | Not_Equal [@printer fun fmt _ -> fprintf fmt "!="]

and logical_op =
  | And [@printer fun fmt _ -> fprintf fmt "&&"]
  | Or [@printer fun fmt _ -> fprintf fmt "||"]
  | Elementwise_And [@printer fun fmt _ -> fprintf fmt "&"]
  | Elementwise_Or [@printer fun fmt _ -> fprintf fmt "|"]

and binary_op =
  | Arithmetic of arithmetic_op [@printer fun fmt o -> fprintf fmt "%s" (show_arithmetic_op o)]
  | Relational of relational_op [@printer fun fmt o -> fprintf fmt "%s" (show_relational_op o)]
  | Logical    of logical_op [@printer fun fmt o -> fprintf fmt "%s" (show_logical_op o)]
  | Seq [@printer fun fmt _ -> fprintf fmt ":"]
[@@deriving eq, show { with_path = false }]

type simple_expression =
  | Lit of literal [@printer fun fmt l -> fprintf fmt "%s" (show_literal l)]
  | Var of identifier [@printer fun fmt -> fprintf fmt "%s"]

and expression =
  | Combine           of simple_expression list
  | Dataframe_Ctor    of (identifier * simple_expression) list
  | Unary_Op          of unary_op * simple_expression
  | Binary_Op         of binary_op * simple_expression * simple_expression
  | Subset1           of simple_expression * simple_expression option
  | Subset2           of simple_expression * simple_expression
  | Call              of identifier * simple_expression list
  | Simple_Expression of simple_expression

and statement =
  | Assign         of identifier * expression
  | Subset1_Assign of identifier * simple_expression option * simple_expression
  | Subset2_Assign of identifier * simple_expression * simple_expression
  | If             of simple_expression * statement list * statement list
  | For            of identifier * simple_expression * statement list
  | Function_Def   of identifier * identifier list * statement list
  | Print          of expression
  | Expression     of expression
[@@deriving eq, show { with_path = false }]

type value =
  | Vector    of literal array * type_tag
  | Dataframe of value array * string array
[@@deriving eq]

let rec show_val = function
  | Vector (a, _) ->
      let inner = a |> Array.map show_literal |> Array.to_list |> String.concat " " in
      "[" ^ inner ^ "]"
  | Dataframe (cols, names) ->
      let inner =
        Array.map2 (fun v n -> n ^ " = " ^ show_val v) cols names
        |> Array.to_list |> String.concat "; " in
      "[" ^ inner ^ "]"

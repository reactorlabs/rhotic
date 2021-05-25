exception Not_supported

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
  | Str s -> "\"" ^ s ^ "\""
  | NA_bool | NA_int | NA_str -> "NA"

type type_tag =
  | T_Bool [@printer fun fmt _ -> fprintf fmt "Bool"]
  | T_Int [@printer fun fmt _ -> fprintf fmt "Int"]
  | T_Str [@printer fun fmt _ -> fprintf fmt "Str"]
[@@deriving eq, show { with_path = false }]

let get_tag = function
  | Bool _ | NA_bool -> T_Bool
  | Int _ | NA_int -> T_Int
  | Str _ | NA_str -> T_Str

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
[@@deriving eq, show { with_path = false }]

type arithmetic_op =
  | Plus
  | Minus
  | Times
  | Int_Divide
  | Modulo
[@@deriving eq, show { with_path = false }]

type relational_op =
  | Less
  | Less_Equal
  | Greater
  | Greater_Equal
  | Equal
  | Not_Equal
[@@deriving eq, show { with_path = false }]

type logical_op =
  | And
  | Or
  | Elementwise_And
  | Elementwise_Or
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Arithmetic of arithmetic_op
  | Relational of relational_op
  | Logical    of logical_op
  | Seq
[@@deriving eq, show { with_path = false }]

type expression =
  | Combine           of simple_expression list
  | Dataframe_Ctor    of (identifier * simple_expression) list
  | Coerce_Op         of type_tag * simple_expression
  | Unary_Op          of unary_op * simple_expression
  | Binary_Op         of binary_op * simple_expression * simple_expression
  | Subset1           of simple_expression * simple_expression option
  | Subset2           of simple_expression * simple_expression
  | Call              of identifier * simple_expression list
  | Simple_Expression of simple_expression
[@@deriving eq, show { with_path = false }]

type statement =
  | Assign         of identifier * expression
  | Subset1_Assign of identifier * simple_expression option * expression
  | Subset2_Assign of identifier * simple_expression * expression
  | Function_Def   of identifier * identifier list * statement list
  | If             of simple_expression * statement list * statement list
  | For            of identifier * simple_expression * statement list
  | Expression     of expression
[@@deriving eq, show { with_path = false }]

type value =
  | Vector    of literal array * type_tag
  | Dataframe of value array * string array
[@@deriving eq]

let rec show_val = function
  | Vector (a, _) ->
      let inner = a |> Array.map show_lit |> Array.to_list |> String.concat " " in
      "[" ^ inner ^ "]"
  | Dataframe (cols, names) ->
      let inner =
        Array.map2 (fun v n -> n ^ " = " ^ show_val v) cols names
        |> Array.to_list |> String.concat "; " in
      "[" ^ inner ^ "]"

let match_vector = function
  | Vector (a, t) -> (a, t)
  | Dataframe _ -> raise Not_supported
let vector_data v = Stdlib.fst @@ match_vector v
let vector_type v = Stdlib.snd @@ match_vector v
let vector_length v = Array.length @@ vector_data v

let vector_of_lit l = Vector ([| l |], get_tag l)
let vector t v = Vector (v, t)

module Env = Map.Make (Identifier)
type environment = value Env.t

module FunTab = Map.Make (Identifier)
type function_table = (identifier list * statement list) FunTab.t

type configuration =
  { env : environment
  ; cur_fun : identifier
  ; fun_tab : function_table
  }

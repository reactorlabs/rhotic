open Util

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

type type_tag =
  | T_Bool [@printer fun fmt _ -> fprintf fmt "Bool"]
  | T_Int [@printer fun fmt _ -> fprintf fmt "Int"]
  | T_Str [@printer fun fmt _ -> fprintf fmt "Str"]
[@@deriving eq, show { with_path = false }]

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
  | Logical_And
  | Logical_Or
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Arithmetic of arithmetic_op
  | Relational of relational_op
  | Logical    of logical_op
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
  | If             of expression * statement list * statement list
  | For            of identifier * expression * statement list
  | Expression     of expression
[@@deriving eq, show { with_path = false }]

type value =
  | Vector    of literal array * type_tag
  | Dataframe of value array * string array
[@@deriving eq]

let rec show_val = function
  | Vector (a, t) ->
      let inner = a |> Array.map show_lit |> Array.to_list |> String.concat " " in
      "[" ^ inner ^ "]," ^ show_type_tag t
  | Dataframe (cols, names) ->
      let inner =
        Array.map2 (fun v n -> n ^ " = " ^ show_val v) cols names
        |> Array.to_list |> String.concat "; " in
      "[" ^ inner ^ "]"

module Env = Map.Make (Identifier)

type environment = value Env.t

(* Helpers for constructing ASTs *)
let get_tag = function
  | Bool _ | NA_bool -> T_Bool
  | Int _ | NA_int -> T_Int
  | Str _ | NA_str -> T_Str

let vec_of_lit l = Vector ([| l |], get_tag l)

let vector t v = Vector (v, t)

module Wrappers = struct
  (* rhotic to OCaml conversion.
     These helpers take a rhotic value and return an OCaml value, wrapped in an Option. None
     represents an NA rhotic value. *)
  let get_bool = function
    | Bool b -> Some b
    | NA_bool -> None
    | Int _ | NA_int | Str _ | NA_str -> assert false
  let get_int = function
    | Int i -> Some i
    | NA_int -> None
    | Bool _ | NA_bool | Str _ | NA_str -> assert false
  let get_str = function
    | Str s -> Some s
    | NA_str -> None
    | Bool _ | NA_bool | Int _ | NA_int -> assert false

  (* OCaml Option to rhotic conversion.
     These helpers take an OCaml value, wrapped in an Option, and return a rhotic value. *)
  let put_bool = function
    | Some b -> Bool b
    | None -> NA_bool
  let put_int = function
    | Some i -> Int i
    | None -> NA_int
  let put_str = function
    | Some s -> Str s
    | None -> NA_str

  (* OCaml to rhotic conversion.
     These helpers take an OCaml value, and return a non-NA rhotic value. *)
  let true_lit = Bool true
  let false_lit = Bool false
  let int_lit i = Int i
  let str_lit s = Str s

  (* Takes a unary OCaml function and a rhotic value.
     The function is total, and does not return None/NA.
     Unwraps the rhotic value, applies the function, then wraps the result. *)
  let map_bool f x = put_bool @@ Option.map f (get_bool x)
  let map_int f x = put_int @@ Option.map f (get_int x)
  let map_str f x = put_str @@ Option.map f (get_str x)

  (* Takes a binary OCaml function and two rhotic values.
     The function is partial, and may return None/NA.
     Unwraps the rhotic values, applies the function, then wraps the result. *)
  let bind2_bool f x y = Option.bind2 f (get_bool x) (get_bool y) |> put_bool
  let bind2_int f x y = Option.bind2 f (get_int x) (get_int y) |> put_int
  let bind2_str f x y = Option.bind2 f (get_str x) (get_str y) |> put_str

  (* Takes a unary OCaml function and a rhotic value.
     The function is partial, and may return None/NA.
     Unwraps the rhotic value, applies the function, then wraps the result. *)
  let bind_bool f x = put_bool @@ Option.bind (get_bool x) f
  let bind_int f x = put_int @@ Option.bind (get_int x) f
  let bind_str f x = put_str @@ Option.bind (get_str x) f

  let na = function
    | T_Bool -> NA_bool
    | T_Int -> NA_int
    | T_Str -> NA_str
end

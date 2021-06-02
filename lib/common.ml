open Expr
open Util

type invalid_number =
  { expected : int
  ; received : int
  }

exception Object_not_found of identifier
exception Function_not_found of identifier
exception Invalid_number_of_args of invalid_number
exception Invalid_argument_type
exception Invalid_subset_index
exception Invalid_subset_replacement
exception Repeated_parameter
exception Vector_lengths_do_not_match
exception Vector_length_greater_one
exception Argument_length_zero
exception Missing_value_need_true_false
exception NA_not_allowed
exception Not_supported

exception Todo

let excptn_to_string = function
  | Object_not_found x -> Printf.sprintf "object '%s' not found" x
  | Function_not_found x -> Printf.sprintf "function '%s' not found" x
  | Invalid_number_of_args { expected; received } ->
      Printf.sprintf "invalid number of arguments, expected %d but received %d" expected received
  | Invalid_argument_type -> "invalid argument type"
  | Invalid_subset_index -> "invalid subset index"
  | Invalid_subset_replacement -> "invalid subset replacement"
  | Repeated_parameter -> "repeated parameter in function definition"
  | Vector_lengths_do_not_match -> "vector lengths do not match"
  | Vector_length_greater_one -> "vector has length > 1"
  | Argument_length_zero -> "argument of length 0"
  | Missing_value_need_true_false -> "missing value where TRUE/FALSE needed"
  | NA_not_allowed -> "NA not allowed"
  | Not_supported -> "not supported"
  | e ->
      Stdlib.prerr_endline "Unrecognized exception" ;
      raise e

module Env = Map.Make (Identifier)
type environment = value Env.t

module FunTab = Map.Make (Identifier)
type function_table = (identifier list * statement list) FunTab.t

type configuration =
  { env : environment
  ; cur_fun : identifier
  ; fun_tab : function_table
  }

let get_tag = function
  | Bool _ | NA_bool -> T_Bool
  | Int _ | NA_int -> T_Int
  | Str _ | NA_str -> T_Str

let na_of = function
  | T_Bool -> NA_bool
  | T_Int -> NA_int
  | T_Str -> NA_str

let is_na = function
  | NA_bool | NA_int | NA_str -> true
  | Bool _ | Int _ | Str _ -> false

let match_vector = function
  | Vector (a, t) -> (a, t)
  | Dataframe _ -> raise Not_supported
let vector_data = Stdlib.fst % match_vector
let vector_type = Stdlib.snd % match_vector
let vector_length = Array.length % vector_data

let vector_of_lit l = Vector ([| l |], get_tag l)
let vector t v = Vector (v, t)

(* We don't have the NULL vector, so for now use an empty boolean vector *)
let null = vector T_Bool [||]

(* rhotic to OCaml conversion.
   These helpers take a rhotic value and return an OCaml value, wrapped in an option. None
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

(* OCaml option to rhotic conversion.
   These helpers take an OCaml value, wrapped in an option, and return a rhotic value. *)
let put_bool = function
  | Some b -> Bool b
  | None -> NA_bool
let put_int = function
  | Some i -> Int i
  | None -> NA_int
let put_str = function
  | Some s -> Str s
  | None -> NA_str

(* Takes an OCaml function that operates on OCaml option values, and lifts it so it operates
   on rhotic values, i.e., it does the rhotic-to-OCaml unwrapping and OCaml-to-rhotic wrapping.

   Note: For flexibility, f must operate on OCaml options, not the raw bool/int/str. This allows
   f to handle None/NA values as inputs *)
let bool, int, str = ((get_bool, put_bool), (get_int, put_int), (get_str, put_str))
let lift (type a) (unwrap, wrap) (f : a option -> a option) (x : literal) = f (unwrap x) |> wrap
let lift2 (type a) (unwrap, wrap) (f : a option -> a option -> a option) (x : literal) (y : literal)
    =
  f (unwrap x) (unwrap y) |> wrap

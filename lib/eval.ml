open Expr
open Util

exception Object_not_found
exception Invalid_argument_type
exception Not_supported

let match_vector = function
  | Vector (data, ty) -> (data, ty)
  | Dataframe _ -> raise Not_supported
let get_vector_data = Stdlib.fst % match_vector
let get_vector_type = Stdlib.snd % match_vector

let lookup env x =
  match Env.find_opt x env with
  | None -> raise Object_not_found
  | Some v -> v

let eval_simple_expr env = function
  | Lit l -> vec_of_lit l
  | Var x -> lookup env x

(* TODO: remove this *)
let empty_vector = Vector ([||], T_Bool)

(* Type hierarchy: T_Bool < T_Int < T_Str *)
let type_lub t1 t2 =
  match (t1, t2) with
  | T_Str, _ | _, T_Str -> T_Str
  | T_Int, _ | _, T_Int -> T_Int
  | T_Bool, _ -> T_Bool

let coerce_value to_ty value =
  match value with
  | Vector (data, from_ty) -> (
      (* NA is NA_int, true is 1, false is 0 *)
      let bool_to_int = Option.map (fun x -> if x then 1 else 0) in

      (* NA is NA_str, true is "TRUE", false is "FALSE" *)
      let bool_to_str = Option.map (fun x -> if x then "TRUE" else "FALSE") in

      (* NA_int is NA, 0 is false, everything else is true *)
      let int_to_bool = Option.map (fun x -> x <> 0) in

      (* NA_int is "NA" *)
      let int_to_str = Option.map Stdlib.string_of_int in

      (* "T" "TRUE" "True" "true" are true, "F" "FALSE" "False" "false" are false, everything else is NA *)
      let str_to_bool s =
        Option.bind s (function
          | "T" | "TRUE" | "True" | "true" -> Some true
          | "F" | "FALSE" | "False" | "false" -> Some false
          | _ -> None) in

      (* Coerce string to int, result is NA if the coercion fails *)
      let str_to_int s = Option.bind s Stdlib.int_of_string_opt in

      let coerce unwrap convert wrap = Array.map (unwrap %> convert %> wrap) data |> vector to_ty in

      let open Wrappers in
      match (from_ty, to_ty) with
      | T_Bool, T_Int -> coerce get_bool bool_to_int put_int
      | T_Bool, T_Str -> coerce get_bool bool_to_str put_str
      | T_Int, T_Bool -> coerce get_int int_to_bool put_bool
      | T_Int, T_Str -> coerce get_int int_to_str put_str
      | T_Str, T_Bool -> coerce get_str str_to_bool put_bool
      | T_Str, T_Int -> coerce get_str str_to_int put_int
      | T_Bool, T_Bool | T_Int, T_Int | T_Str, T_Str -> value )
  | Dataframe _ -> raise Not_supported

let combine values =
  (* Get the least upper bound of all types
     Then coerce all vectors to that type, extract, and concatenate the data *)
  let ty = values |> List.map get_vector_type |> List.fold_left type_lub T_Bool in
  let data = values |> List.map @@ coerce_value ty |> List.map get_vector_data |> Array.concat in
  vector ty data

(* Boolean and integer values get coerced; strings cannot be coerced.
   Unary operations on data frames are not supported. *)
let unary op v =
  let open Wrappers in
  if get_vector_type v = T_Str then raise Invalid_argument_type ;
  match op with
  | Logical_Not ->
      (* Coerce to boolean, apply logical not *)
      v |> coerce_value T_Bool |> get_vector_data
      |> Array.map @@ map_bool (fun x -> not x)
      |> vector T_Bool
  | Unary_Plus ->
      (* Nop for integers, but coerces booleans to integers *)
      v |> coerce_value T_Int
  | Unary_Minus ->
      (* Coerce to integer, apply unary negation *)
      v |> coerce_value T_Int |> get_vector_data
      |> Array.map @@ map_int (fun x -> -x)
      |> vector T_Int

let eval_expr env = function
  | Combine [] -> vector T_Bool [||]
  | Combine ses -> ses |> List.map @@ eval_simple_expr env |> combine
  | Dataframe_Ctor _ -> raise Not_supported
  | Coerce_Op (ty, se) -> se |> eval_simple_expr env |> coerce_value ty
  | Unary_Op (op, se) -> se |> eval_simple_expr env |> unary op
  | Binary_Op (_, _, _) -> empty_vector
  | Subset1 (_, _) -> empty_vector
  | Subset2 (_, _) -> empty_vector
  | Call (_, _) -> empty_vector
  | Simple_Expression se -> eval_simple_expr env se

let[@warning "-4-8"] eval_string s =
  match Parse.parse s with
  | [ Expression e ] -> print_endline @@ show_val @@ eval_expr Env.empty e
  | _ -> ()

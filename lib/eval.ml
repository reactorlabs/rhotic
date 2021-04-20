open Expr
open Util

exception Object_not_found
exception Coercion_not_possible
exception Not_supported

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
  let open Wrappers in
  match value with
  | Vector (data, from_ty) -> (
      (* NA is NA_int, true is 1, false is 0 *)
      let bool_to_int =
        Option.map (function
          | true -> 1
          | false -> 0) in

      (* NA is NA_str, true is "TRUE", false is "FALSE" *)
      let bool_to_str =
        Option.map (function
          | true -> "TRUE"
          | false -> "FALSE") in

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

      let coerce unwrap convert wrap =
        let res = Array.map (unwrap %> convert %> wrap) data in
        vector res to_ty in

      match (from_ty, to_ty) with
      | T_Bool, T_Int -> coerce get_bool bool_to_int put_int
      | T_Bool, T_Str -> coerce get_bool bool_to_str put_str
      | T_Int, T_Bool -> coerce get_int int_to_bool put_bool
      | T_Int, T_Str -> coerce get_int int_to_str put_str
      | T_Str, T_Bool -> coerce get_str str_to_bool put_bool
      | T_Str, T_Int -> coerce get_str str_to_int put_int
      | T_Bool, T_Bool | T_Int, T_Int | T_Str, T_Str -> value )
  | Dataframe _ -> raise Coercion_not_possible

let eval_expr env = function
  | Combine [] -> Vector ([||], T_Bool)
  | Combine ses ->
      let values = List.map (eval_simple_expr env) ses in
      (* Get the least upper bound of all types *)
      let ty =
        values
        |> List.map (function
             | Vector (_, t) -> t
             | Dataframe _ -> raise Not_supported)
        |> List.fold_left type_lub T_Bool in
      (* Coerce all vectors to that type, then extract and concatenate the data *)
      let data =
        values
        |> List.map (coerce_value ty)
        |> List.map (function
             | Vector (a, _) -> a
             | Dataframe _ -> raise Not_supported)
        |> Array.concat in
      Vector (data, ty)
  | Dataframe_Ctor _ -> raise Not_supported
  | Coerce_Op (ty, se) ->
      let v = eval_simple_expr env se in
      coerce_value ty v
  | Unary_Op (_, _) -> empty_vector
  | Binary_Op (_, _, _) -> empty_vector
  | Subset1 (_, _) -> empty_vector
  | Subset2 (_, _) -> empty_vector
  | Call (_, _) -> empty_vector
  | Simple_Expression se -> eval_simple_expr env se
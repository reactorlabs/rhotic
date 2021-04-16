open Expr

exception Object_not_found
exception Coercion_not_possible

(* Gets a boolean value from a bool Literal.
   Wraps the result in an Option, so that NA is represented by None. *)
let get_bool = function
  | Bool b -> Some b
  | NA_bool -> None
  | Int _ | NA_int | Str _ | NA_str -> assert false

(* Gets an integer value from an int Literal.
   Wraps the result in an Option, so that NA is represented by None. *)
let get_int = function
  | Int i -> Some i
  | NA_int -> None
  | Bool _ | NA_bool | Str _ | NA_str -> assert false

(* Gets a string value from a str Literal.
   Wraps the result in an Option, so that NA is represented by None. *)
let get_str = function
  | Str s -> Some s
  | NA_str -> None
  | Bool _ | NA_bool | Int _ | NA_int -> assert false

let lookup env x =
  match Env.find_opt x env with
  | None -> raise Object_not_found
  | Some v -> v

let eval_simple_expr env = function
  | Lit l -> vec_of_lit l
  | Var x -> lookup env x

(* TODO: remove this *)
let empty_vector = Vector ([||], T_Bool)

let coerce_value to_ty v =
  match v with
  | Vector (a, from_ty) -> (
      match (to_ty, from_ty) with
      | T_Bool, T_Int ->
          let res =
            a |> Array.map get_int
            |> Array.map (Option.map (fun x -> x <> 0))
            |> Array.map opt_bool_lit in
          vector res to_ty
      | T_Bool, T_Str -> empty_vector
      | T_Int, T_Bool -> empty_vector
      | T_Int, T_Str -> empty_vector
      | T_Str, T_Bool -> empty_vector
      | T_Str, T_Int -> empty_vector
      | T_Bool, T_Bool | T_Int, T_Int | T_Str, T_Str -> v )
  | Dataframe _ -> raise Coercion_not_possible

let eval_expr env = function
  | Combine [] -> Vector ([||], T_Bool)
  | Combine _ -> empty_vector
  | Dataframe_Ctor _ -> empty_vector
  | Coerce_Op (ty, se) ->
      let v = eval_simple_expr env se in
      coerce_value ty v
  | Unary_Op (_, _) -> empty_vector
  | Binary_Op (_, _, _) -> empty_vector
  | Subset1 (_, _) -> empty_vector
  | Subset2 (_, _) -> empty_vector
  | Call (_, _) -> empty_vector
  | Simple_Expression se -> eval_simple_expr env se

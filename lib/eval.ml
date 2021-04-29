open Expr
open Util

exception Todo

exception Object_not_found
exception Invalid_argument_type
exception Vector_lengths_do_not_match
exception Not_supported

let excptn_to_string = function
  | Object_not_found -> "object not found"
  | Invalid_argument_type -> "invalid argument type"
  | Vector_lengths_do_not_match -> "vector lengths do not match"
  | Not_supported -> "not supported"
  | e ->
      Stdlib.prerr_endline "Unrecognized exception" ;
      raise e

let match_vector = function
  | Vector (data, ty) -> (data, ty)
  | Dataframe _ -> raise Not_supported
let vector_data = Stdlib.fst % match_vector
let vector_type = Stdlib.snd % match_vector
let vector_length = Array.length % vector_data

let get_tag = function
  | Bool _ | NA_bool -> T_Bool
  | Int _ | NA_int -> T_Int
  | Str _ | NA_str -> T_Str

let vector_of_lit l = Vector ([| l |], get_tag l)
let vector t v = Vector (v, t)

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

let lookup env x =
  match Env.find_opt x env with
  | None -> raise Object_not_found
  | Some v -> v

let eval_simple_expr env = function
  | Lit l -> vector_of_lit l
  | Var x -> lookup env x

(* Type hierarchy: T_Bool < T_Int < T_Str *)
let type_lub t1 t2 =
  match (t1, t2) with
  | T_Str, _ | _, T_Str -> T_Str
  | T_Int, _ | _, T_Int -> T_Int
  | T_Bool, _ -> T_Bool

let coerce_data from_ty to_ty data =
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

  let coerce unwrap convert wrap = Array.map (unwrap %> convert %> wrap) data in
  match (from_ty, to_ty) with
  | T_Bool, T_Int -> coerce get_bool bool_to_int put_int
  | T_Bool, T_Str -> coerce get_bool bool_to_str put_str
  | T_Int, T_Bool -> coerce get_int int_to_bool put_bool
  | T_Int, T_Str -> coerce get_int int_to_str put_str
  | T_Str, T_Bool -> coerce get_str str_to_bool put_bool
  | T_Str, T_Int -> coerce get_str str_to_int put_int
  | T_Bool, T_Bool | T_Int, T_Int | T_Str, T_Str -> data

let coerce_value to_ty = function
  | Vector (data, from_ty) -> coerce_data from_ty to_ty data |> vector to_ty
  | Dataframe _ -> raise Not_supported

let combine values =
  (* Get the least upper bound of all types
     Then coerce all vectors to that type, extract, and concatenate the data *)
  let ty = values |> List.map vector_type |> List.fold_left type_lub T_Bool in
  let data = values |> List.map (coerce_value ty) |> List.map vector_data |> Array.concat in
  vector ty data

(* Boolean and integer values get coerced; strings cannot be coerced.
   Unary operations on data frames are not supported. *)
let unary op = function
  | Vector (data, ty) -> (
      if ty = T_Str then raise Invalid_argument_type ;
      match op with
      | Logical_Not ->
          (* Coerce to boolean, apply logical not *)
          data |> coerce_data ty T_Bool |> Array.map (lift bool @@ Option.map not) |> vector T_Bool
      | Unary_Plus ->
          (* Nop for integers, but coerces booleans to integers *)
          data |> coerce_data ty T_Int |> vector T_Int
      | Unary_Minus ->
          (* Coerce to integer, apply unary negation *)
          data |> coerce_data ty T_Int |> Array.map (lift int @@ Option.map ( ~- )) |> vector T_Int
      )
  | Dataframe _ -> raise Not_supported

let binary op v1 v2 =
  match (v1, v2) with
  | Vector (data1, ty1), Vector (data2, ty2) -> (
      (* Both vectors have to have the same length. *)
      if vector_length v1 <> vector_length v2 then raise Vector_lengths_do_not_match ;

      match op with
      | Arithmetic o -> (
          (* String operands not allowed; but coerce booleans to integers. *)
          if ty1 = T_Str || ty2 = T_Str then raise Invalid_argument_type ;
          let data1 = data1 |> coerce_data ty1 T_Int in
          let data2 = data2 |> coerce_data ty2 T_Int in

          (* R uses "floored" modulo while OCaml uses "truncated" modulo.
             E.g.: 5 %% -2 == -1 in R, but 5 mod -2 == 1 in OCaml *)
          let div' x y = float_of_int x /. float_of_int y |> floor |> int_of_float in
          let mod' x y = x - (y * div' x y) in

          let arithmetic f = Array.map2 (lift2 int @@ Option.bind2 f) data1 data2 |> vector T_Int in
          match o with
          | Plus -> arithmetic (fun x y -> Some (x + y))
          | Minus -> arithmetic (fun x y -> Some (x - y))
          | Times -> arithmetic (fun x y -> Some (x * y))
          | Int_Divide -> arithmetic (fun x y -> if y = 0 then None else Some (div' x y))
          | Modulo -> arithmetic (fun x y -> if y = 0 then None else Some (mod' x y)) )
      | Relational o -> (
          (* Bools and ints use numeric comparisons, while strings use lexicographic comparisons.
             We need to properly coerce the operands, but also need to handle numeric values and
             string values differently. *)
          match (ty1, ty2) with
          | T_Str, _ | _, T_Str -> (
              let data1 = data1 |> coerce_data ty1 T_Str in
              let data2 = data2 |> coerce_data ty2 T_Str in
              let relational f =
                Array.map2
                  (fun x y -> (Option.bind2 f) (get_str x) (get_str y) |> put_bool)
                  data1 data2
                |> vector T_Bool in
              match o with
              | Less -> relational (fun x y -> Some (String.compare x y < 0))
              | Less_Equal -> relational (fun x y -> Some (String.compare x y <= 0))
              | Greater -> relational (fun x y -> Some (String.compare x y > 0))
              | Greater_Equal -> relational (fun x y -> Some (String.compare x y >= 0))
              | Equal -> relational (fun x y -> Some (String.compare x y = 0))
              | Not_Equal -> relational (fun x y -> Some (String.compare x y <> 0)) )
          | T_Int, _ | _, T_Int | T_Bool, _ -> (
              let data1 = data1 |> coerce_data ty1 T_Int in
              let data2 = data2 |> coerce_data ty2 T_Int in
              let relational f =
                Array.map2
                  (fun x y -> (Option.bind2 f) (get_int x) (get_int y) |> put_bool)
                  data1 data2
                |> vector T_Bool in
              match o with
              | Less -> relational (fun x y -> Some (x < y))
              | Less_Equal -> relational (fun x y -> Some (x <= y))
              | Greater -> relational (fun x y -> Some (x > y))
              | Greater_Equal -> relational (fun x y -> Some (x >= y))
              | Equal -> relational (fun x y -> Some (x = y))
              | Not_Equal -> relational (fun x y -> Some (x <> y)) ) )
      | Logical o -> (
          (* String operands not allowed; but coerce integers to booleans. *)
          if ty1 = T_Str || ty2 = T_Str then raise Invalid_argument_type ;
          let data1 = data1 |> coerce_data ty1 T_Bool in
          let data2 = data2 |> coerce_data ty2 T_Bool in

          (* Logical comparisons use three-valued logic, e.g. T && NA == NA, but F && NA == F. *)
          let and' x y =
            match (x, y) with
            | Some true, Some true -> Some true
            | Some false, _ | _, Some false -> Some false
            | _ -> None in
          let or' x y =
            match (x, y) with
            | Some false, Some false -> Some false
            | Some true, _ | _, Some true -> Some true
            | _ -> None in

          (* And and Or compare the first element of each vector; empty vector is treated as NA. *)
          let elementwise f = Array.map2 (lift2 bool f) data1 data2 |> vector T_Bool in
          let e1 = if Array.length data1 = 0 then None else get_bool data1.(0) in
          let e2 = if Array.length data2 = 0 then None else get_bool data2.(0) in
          match o with
          | And -> and' e1 e2 |> put_bool |> vector_of_lit
          | Or -> or' e1 e2 |> put_bool |> vector_of_lit
          | Elementwise_And -> elementwise and'
          | Elementwise_Or -> elementwise or' ) )
  | Vector _, _ | _, Vector _ | Dataframe _, _ -> raise Not_supported

(* TODO: takes a single int index *)
let subset2 v1 _ = v1

let eval_expr env expr =
  let eval = eval_simple_expr env in
  match expr with
  | Combine [] -> vector T_Bool [||]
  | Combine ses -> combine @@ List.map eval ses
  | Dataframe_Ctor _ -> raise Not_supported
  | Coerce_Op (ty, se) -> coerce_value ty (eval se)
  | Unary_Op (op, se) -> unary op (eval se)
  | Binary_Op (op, se1, se2) -> binary op (eval se1) (eval se2)
  | Subset1 (_, _) ->
      (* TODO: Vector of positive indices of arbitrary length
         Vector of boolean indices of vector length
         No recycling *)
      raise Todo
  | Subset2 (se1, se2) -> subset2 (eval se1) (eval se2)
  | Call (_, _) -> raise Todo
  | Simple_Expression se -> eval se

let eval_stmt env stmt =
  let eval = eval_expr env in
  match stmt with
  | Assign (x, e) ->
      let v = eval e in
      let env' = Env.add x v env in
      (env', v)
  | Subset1_Assign (_, _, _) -> raise Todo
  | Subset2_Assign (_, _, _) -> raise Todo
  | Function_Def (_, _, _) ->
      (* TODO: Restrict parser so that functions can only be defined at top level *)
      raise Todo
  | If (_, _, _) -> raise Todo
  | For (_, _, _) -> raise Todo
  | Expression e -> (env, eval e)

let rec run_program env (stmts : statement list) =
  match stmts with
  | [] -> (env, vector T_Bool [||])
  | [ stmt ] -> eval_stmt env stmt
  | stmt :: stmts ->
      let env', _ = eval_stmt env stmt in
      (run_program [@tailcall]) env' stmts

(* TODO: Program state
    program counter
    program (array of statements)
    environment
    current function
    function table
    stack of (return value, env, return pc)
*)

let run s =
  let program = Parse.parse s in
  print_endline @@ show_val @@ Stdlib.snd @@ run_program Env.empty program

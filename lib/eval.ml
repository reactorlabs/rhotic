open Expr
open Common
open Util

let lookup env x =
  match Env.find_opt x env with
  | None -> raise (Object_not_found x)
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

let check_type ty = function
  | Vector (data, vector_ty) ->
      assert (Array.for_all (fun x -> get_tag x = vector_ty) data) ;
      Some (ty = vector_ty) |> put_bool |> vector_of_lit
  | Dataframe _ -> raise Not_supported

let combine values =
  (* Get the least upper bound of all types
     Then coerce all vectors to that type, extract, and concatenate the data *)
  let ty = values |> List.map vector_type |> List.fold_left type_lub T_Bool in
  let data = values |> List.map (coerce_value ty) |> List.map vector_data |> Array.concat in
  vector ty data

(* Boolean and integer values get coerced for Logical_Not, Unary_Plus, and Unary_Minus;
   strings cannot be coerced. Unary operations on data frames are not supported. *)
let unary op = function
  | Vector (a, t) as v -> (
      match op with
      | Logical_Not ->
          (* Coerce to boolean, apply logical not *)
          if t = T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Bool |> Array.map (lift bool @@ Option.map not) |> vector T_Bool
      | Unary_Plus ->
          (* Nop for integers, but coerces booleans to integers *)
          if t = T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Int |> vector T_Int
      | Unary_Minus ->
          (* Coerce to integer, apply unary negation *)
          if t = T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Int |> Array.map (lift int @@ Option.map ( ~- )) |> vector T_Int
      | As_Logical -> coerce_value T_Bool v
      | As_Integer -> coerce_value T_Int v
      | As_Character -> coerce_value T_Str v
      | Is_Logical -> check_type T_Bool v
      | Is_Integer -> check_type T_Int v
      | Is_Character -> check_type T_Str v
      | Is_NA -> a |> Array.map (is_na %> Option.some %> put_bool) |> vector T_Bool)
  | Dataframe _ -> raise Not_supported

let binary op v1 v2 =
  let arithmetic_op o =
    let a1, a2 = (vector_data v1, vector_data v2) in
    let t1, t2 = (vector_type v1, vector_type v2) in

    (* String operands not allowed; but coerce booleans to integers. *)
    if t1 = T_Str || t2 = T_Str then raise Invalid_argument_type ;
    let a1 = a1 |> coerce_data t1 T_Int in
    let a2 = a2 |> coerce_data t2 T_Int in

    (* R uses "floored" modulo while OCaml uses "truncated" modulo.
        E.g.: 5 %% -2 == -1 in R, but 5 mod -2 == 1 in OCaml *)
    let div' x y = float_of_int x /. float_of_int y |> floor |> int_of_float in
    let mod' x y = x - (y * div' x y) in

    let arithmetic f = Array.map2 (lift2 int @@ Option.bind2 f) a1 a2 |> vector T_Int in
    match o with
    | Plus -> arithmetic (fun x y -> Some (x + y))
    | Minus -> arithmetic (fun x y -> Some (x - y))
    | Times -> arithmetic (fun x y -> Some (x * y))
    | Int_Divide -> arithmetic (fun x y -> if y = 0 then None else Some (div' x y))
    | Modulo -> arithmetic (fun x y -> if y = 0 then None else Some (mod' x y)) in

  let relational_op o =
    let a1, a2 = (vector_data v1, vector_data v2) in
    let t1, t2 = (vector_type v1, vector_type v2) in

    (* Bools and ints use numeric comparisons, while strings use lexicographic comparisons.
        We need to properly coerce the operands, but also need to handle numeric values and string
        values differently. *)
    match (t1, t2) with
    | T_Str, _ | _, T_Str -> (
        let a1 = a1 |> coerce_data t1 T_Str in
        let a2 = a2 |> coerce_data t2 T_Str in
        let relational f =
          Array.map2 (fun x y -> (Option.bind2 f) (get_str x) (get_str y) |> put_bool) a1 a2
          |> vector T_Bool in
        match o with
        | Less -> relational (fun x y -> Some (String.compare x y < 0))
        | Less_Equal -> relational (fun x y -> Some (String.compare x y <= 0))
        | Greater -> relational (fun x y -> Some (String.compare x y > 0))
        | Greater_Equal -> relational (fun x y -> Some (String.compare x y >= 0))
        | Equal -> relational (fun x y -> Some (String.compare x y = 0))
        | Not_Equal -> relational (fun x y -> Some (String.compare x y <> 0)))
    | T_Int, _ | _, T_Int | T_Bool, _ -> (
        let a1 = a1 |> coerce_data t1 T_Int in
        let a2 = a2 |> coerce_data t2 T_Int in
        let relational f =
          Array.map2 (fun x y -> (Option.bind2 f) (get_int x) (get_int y) |> put_bool) a1 a2
          |> vector T_Bool in
        match o with
        | Less -> relational (fun x y -> Some (x < y))
        | Less_Equal -> relational (fun x y -> Some (x <= y))
        | Greater -> relational (fun x y -> Some (x > y))
        | Greater_Equal -> relational (fun x y -> Some (x >= y))
        | Equal -> relational (fun x y -> Some (x = y))
        | Not_Equal -> relational (fun x y -> Some (x <> y))) in

  let logical_op o =
    let a1, a2 = (vector_data v1, vector_data v2) in
    let t1, t2 = (vector_type v1, vector_type v2) in

    (* String operands not allowed; but coerce integers to booleans. *)
    if t1 = T_Str || t2 = T_Str then raise Invalid_argument_type ;
    let a1 = a1 |> coerce_data t1 T_Bool in
    let a2 = a2 |> coerce_data t2 T_Bool in

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
    let elementwise f = Array.map2 (lift2 bool f) a1 a2 |> vector T_Bool in
    let e1 = if Array.length a1 = 0 then None else get_bool a1.(0) in
    let e2 = if Array.length a2 = 0 then None else get_bool a2.(0) in
    match o with
    | And -> and' e1 e2 |> put_bool |> vector_of_lit
    | Or -> or' e1 e2 |> put_bool |> vector_of_lit
    | Elementwise_And -> elementwise and'
    | Elementwise_Or -> elementwise or' in

  (* This needs to be a function, not a constant, because it might raise an exception *)
  let sequence_op () =
    let a1, a2 = (vector_data v1, vector_data v2) in
    let t1, t2 = (vector_type v1, vector_type v2) in

    if Array.length a1 = 0 || Array.length a2 = 0 then raise Argument_length_zero ;
    if Array.length a1 > 1 || Array.length a2 > 1 then raise Vector_length_greater_one ;

    (* Everything gets coerced to integer *)
    let a1 = a1 |> coerce_data t1 T_Int |> Array.map get_int in
    let a2 = a2 |> coerce_data t2 T_Int |> Array.map get_int in

    match (a1.(0), a2.(0)) with
    | Some e1, Some e2 ->
        (* We actually want the opposite sign of Stdlib.compare: + if e1 < e2 *)
        let sign = Stdlib.compare e2 e1 in
        let len = Stdlib.abs (e2 - e1) + 1 in
        let res = Array.make len None in
        for i = 0 to len - 1 do
          res.(i) <- Some (e1 + (sign * i))
        done ;
        res |> Array.map put_int |> vector T_Int
    | None, None | None, _ | _, None -> raise NA_not_allowed in

  match (v1, v2) with
  | Vector _, Vector _ -> (
      (* Both vectors must have the same length. *)
      if vector_length v1 <> vector_length v2 then raise Vector_lengths_do_not_match ;
      match op with
      | Arithmetic o -> arithmetic_op o
      | Relational o -> relational_op o
      | Logical o -> logical_op o
      | Seq -> sequence_op ())
  | Vector _, _ | _, Vector _ | Dataframe _, _ -> raise Not_supported

(* Checks that all elements are non-negative or NA.
  0 and NA are allowed for positive subsetting. *)
let is_positive_subsetting = Array.for_all @@ Option.map_or ~default:true (fun x -> x >= 0)

let is_zero_subsetting = Array.for_all (fun x -> x = Some 0)

let contains_na (type t) (a : t option array) = Array.exists (fun x -> x = None) a

(* Uses the indices in `idx` to select elements out of the array `a`:
  - Valid indices are converted from 1-based indexing (R) to 0-based indexing (OCaml).
  - 0 indices are dropped.
  - Out-of-bounds and NA indices return NA.
  Expects the inputs to be valid. *)
let get_at_pos ty a idxs =
  assert (is_positive_subsetting idxs) ;
  idxs
  |> Array.filter_map (function
       | Some i when 1 <= i && i <= Array.length a -> Some a.(i - 1)
       | Some 0 -> None
       | Some _ | None -> Some (na_of ty))

(* Convert a boolean vector (used for subsetting) to a positional vector:
  - True indices are converted to positional indices.
  - False indices are dropped.
  - NA indices stay as NA. *)
let bool_to_pos_vector idxs =
  idxs
  |> Array.filter_mapi (fun i x ->
         match x with
         | Some true -> Some (Some (i + 1))
         | Some false -> None
         | None -> Some None)

let extend n t a =
  let m = Array.length a in
  if m >= n then Array.copy a
  else
    let res = Array.make n (na_of t) in
    for i = 0 to m - 1 do
      (* Only copy over m elements from a to res; leave the rest as NA. *)
      res.(i) <- a.(i)
    done ;
    res

let update_at_pos t a idxs rpl =
  assert (is_positive_subsetting idxs) ;
  assert (Array.for_all (fun x -> x <> Some 0) idxs) ;
  assert (not @@ contains_na idxs) ;
  let idxs = Array.map Option.get idxs in
  let max_idx = Array.fold_left (fun mx i -> max mx i) (Array.length a) idxs in
  let res = extend max_idx t a in
  Array.iter2 (fun i x -> res.(i - 1) <- x) idxs rpl ;
  res

let subset1 v1 v2 =
  match (v1, v2) with
  | Vector (a1, t1), Vector (a2, t2) -> (
      match t2 with
      | T_Bool ->
          (* Both vectors must have the same length. *)
          if vector_length v1 <> vector_length v2 then raise Vector_lengths_do_not_match ;
          a2 |> Array.map get_bool |> bool_to_pos_vector |> get_at_pos t1 a1 |> vector t1
      | T_Int ->
          let a2 = a2 |> Array.map get_int in
          if not @@ is_positive_subsetting a2 then raise Invalid_subset_index ;
          a2 |> get_at_pos t1 a1 |> vector t1
      | T_Str -> raise Invalid_argument_type)
  | Dataframe _, _ -> raise Not_supported
  | _, Dataframe _ -> raise Invalid_argument_type

let subset2 v1 v2 =
  match (v1, v2) with
  | Vector (a1, _), Vector (a2, t2) -> (
      let n1, n2 = (vector_length v1, vector_length v2) in
      if n2 = 0 || n2 > 1 || t2 = T_Str then raise Invalid_subset_index ;
      let a2 = a2 |> coerce_data t2 T_Int in
      match get_int a2.(0) with
      | Some i when 1 <= i && i <= n1 -> vector_of_lit a1.(i - 1)
      | Some _ | None -> raise Invalid_subset_index)
  | Dataframe _, _ -> raise Not_supported
  | _, Dataframe _ -> raise Invalid_argument_type

let subset1_assign conf x idx v =
  match (lookup conf.env x, idx, v) with
  | (Vector _ as v1), None, (Vector _ as v3) ->
      if vector_length v1 <> vector_length v3 then raise Vector_lengths_do_not_match ;
      let conf' = { conf with env = Env.add x v3 conf.env } in
      (conf', v3)
  | (Vector (a1, t1) as v1), Some (Vector (a2, t2) as v2), (Vector (a3, t3) as v3) -> (
      let n1, n2, n3 = (vector_length v1, vector_length v2, vector_length v3) in
      let t = type_lub t1 t3 in
      let a1, a3 = (a1 |> coerce_data t1 t, a3 |> coerce_data t3 t) in
      match t2 with
      | T_Bool ->
          let a2 = a2 |> Array.map get_bool in
          if contains_na a2 then raise Invalid_subset_index ;
          if n1 <> n2 then raise Vector_lengths_do_not_match ;
          let a2 = a2 |> bool_to_pos_vector in
          let n2 = Array.length a2 in
          if n2 <> n3 then raise Invalid_subset_replacement ;
          let res = update_at_pos t a1 a2 a3 in
          let conf' = { conf with env = Env.add x (vector t res) conf.env } in
          (conf', v3)
      | T_Int ->
          let a2 = a2 |> Array.map get_int in
          if contains_na a2 || (not @@ is_positive_subsetting a2) then raise Invalid_subset_index ;
          if is_zero_subsetting a2 then
            let conf' = { conf with env = Env.add x (vector t a1) conf.env } in
            (conf', v3)
          else
            let a2 = a2 |> Array.filter (fun x -> x <> Some 0) in
            let n2 = Array.length a2 in
            if n2 <> n3 then raise Invalid_subset_replacement ;
            let res = update_at_pos t a1 a2 a3 in
            let conf' = { conf with env = Env.add x (vector t res) conf.env } in
            (conf', v3)
      | T_Str -> raise Invalid_argument_type)
  | Dataframe _, _, _ -> raise Not_supported
  | _, Some (Dataframe _), _ | _, _, Dataframe _ -> raise Invalid_argument_type

let subset2_assign conf x idx v =
  match (lookup conf.env x, idx, v) with
  | Vector (a1, t1), (Vector (a2, t2) as v2), (Vector (a3, t3) as v3) -> (
      let n2, n3 = (vector_length v2, vector_length v3) in
      if n2 = 0 || n2 > 1 || t2 = T_Str then raise Invalid_subset_index ;
      if n3 = 0 || n3 > 1 then raise Invalid_subset_replacement ;
      let t = type_lub t1 t3 in
      let a1, a3 = (a1 |> coerce_data t1 t, a3 |> coerce_data t3 t) in
      let a2 = a2 |> coerce_data t2 T_Int in
      match get_int a2.(0) with
      | Some i when 1 <= i ->
          let a1 = extend i t a1 in
          a1.(i - 1) <- a3.(0) ;
          let conf' = { conf with env = Env.add x (vector t a1) conf.env } in
          (conf', v3)
      | Some _ | None -> raise Invalid_subset_index)
  | Dataframe _, _, _ -> raise Not_supported
  | _, Dataframe _, _ | _, _, Dataframe _ -> raise Invalid_argument_type

let rec eval_expr monitors conf expr =
  let run_stmts conf stmts = run_statements monitors conf stmts in
  let eval = eval_simple_expr conf.env in

  let eval_call id args =
    match FunTab.find_opt id conf.fun_tab with
    | None -> raise (Function_not_found id)
    | Some (params, stmts) ->
        let n1, n2 = (List.length args, List.length params) in
        if n1 <> n2 then raise (Invalid_number_of_args { expected = n2; received = n1 }) ;
        let fun_env = List.fold_left2 (fun e x v -> Env.add x v e) Env.empty params args in
        let conf' = { conf with env = fun_env; cur_fun = id } in
        Stdlib.snd @@ run_stmts conf' stmts in

  match expr with
  | Combine [] -> null
  | Combine ses -> combine @@ List.map eval ses
  | Dataframe_Ctor _ -> raise Not_supported
  | Unary_Op (op, se) -> unary op (eval se)
  | Binary_Op (op, se1, se2) -> binary op (eval se1) (eval se2)
  | Subset1 (se1, None) -> eval se1
  | Subset1 (se1, Some se2) -> subset1 (eval se1) (eval se2)
  | Subset2 (se1, se2) -> subset2 (eval se1) (eval se2)
  | Call (id, ses) ->
      let args = List.map eval ses in
      let res = eval_call id args in
      List.iter (fun m -> m#record_call conf id ses args res) monitors ;
      res
  | Simple_Expression se -> eval se

and eval_stmt monitors conf stmt =
  let run_stmts conf stmts = run_statements monitors conf stmts in
  let eval = eval_expr monitors conf in
  let eval_se = eval_simple_expr conf.env in

  let eval_fun_def id params stmts =
    let unique_params = List.sort_uniq String.compare params in
    if List.length params <> List.length unique_params then raise Repeated_parameter ;
    let conf' = { conf with fun_tab = FunTab.add id (params, stmts) conf.fun_tab } in
    (conf', null) in

  let eval_if se1 s2 s3 =
    let cond = eval_se se1 in
    List.iter (fun m -> m#record_if conf se1 s2 s3 cond) monitors ;
    match cond with
    | Vector _ as v1 -> (
        if vector_length v1 = 0 then raise Argument_length_zero ;
        if vector_length v1 > 1 then raise Vector_length_greater_one ;
        let a1 = v1 |> coerce_value T_Bool |> vector_data |> Array.map get_bool in
        match a1.(0) with
        | None -> raise Missing_value_need_true_false
        | Some true -> run_stmts conf s2
        | Some false -> run_stmts conf s3)
    | Dataframe _ -> raise Invalid_argument_type in

  let eval_for var seq stmts =
    match seq with
    | Vector (a, _) ->
        let loop conf i =
          let conf' = { conf with env = Env.add var (vector_of_lit i) conf.env } in
          Stdlib.fst @@ run_stmts conf' stmts in
        let conf' = Array.fold_left loop conf a in
        (conf', null)
    | Dataframe _ -> raise Not_supported in

  match stmt with
  | Assign (x, e) ->
      let v = eval e in
      let conf' = { conf with env = Env.add x v conf.env } in
      (conf', v)
  | Subset1_Assign (x1, se2, se3) -> subset1_assign conf x1 (Option.map eval_se se2) (eval_se se3)
  | Subset2_Assign (x1, se2, se3) -> subset2_assign conf x1 (eval_se se2) (eval_se se3)
  | Function_Def (id, params, stmts) ->
      let res = eval_fun_def id params stmts in
      List.iter (fun m -> m#record_fun_def conf id params stmts) monitors ;
      res
  | If (se1, s2, s3) -> eval_if se1 s2 s3
  | For (x1, se2, s3) -> eval_for x1 (eval_se se2) s3
  | Expression e -> (conf, eval e)

and run_statements (monitors : Monitor.monitors) (conf : configuration) (stmts : statement list) =
  match stmts with
  | [] ->
      (* This is to handle the empty program *)
      (conf, null)
  | [ stmt ] ->
      (* This is the base case; we want to return a value *)
      let res = eval_stmt monitors conf stmt in
      res
  | stmt :: stmts ->
      let conf', _ = eval_stmt monitors conf stmt in
      (run_statements [@tailcall]) monitors conf' stmts

let start = { env = Env.empty; cur_fun = "main$"; fun_tab = FunTab.empty }

let run ?(monitors : Monitor.monitors = []) str =
  let program = Parse.parse str in
  Stdlib.print_endline @@ show_val @@ Stdlib.snd @@ run_statements monitors start program

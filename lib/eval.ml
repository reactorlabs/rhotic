open Containers
open CCFun.Infix
open Common
open Util
open Opcode

module E = Expr

module State = struct
  module Env = Map.Make (E.Identifier)
  type environment = E.value Env.t
  type frame = pc * E.identifier * environment
  type state =
    { program : opcode Vector.ro_vector
    ; pc : pc
    ; last_val : E.value option
    ; env : environment [@default Env.empty]
    ; stack : frame list
    }
  [@@deriving make]
  type advance_type =
    | Next
    | Jump of pc

  let current_op state = Vector.get state.program state.pc

  let lookup x state =
    match Env.find_opt x state.env with
    | Some v -> v
    | None -> raise (Object_not_found x)

  (* Updating the environment will automatically update state.last_val, but providing
     an optional parameter will override and set state.last_val. *)
  let update x v ?(last_val = None) state =
    let env = Env.add x v state.env in
    match last_val with
    | None -> { state with env; last_val = Some v }
    | Some vv -> { state with env; last_val = vv }

  let clear_last_val state = { state with last_val = None }

  let last_val state = state.last_val

  let advance_pc which state =
    match which with
    | Next -> { state with pc = state.pc + 1 }
    | Jump pc -> { state with pc }

  let push target fn_pc params args state =
    let pc = state.pc + 1 in
    let stack = (pc, target, state.env) :: state.stack in
    let env = List.fold_left2 (fun e x v -> Env.add x v e) Env.empty params args in
    { state with pc = fn_pc; env; stack }

  let pop state =
    let (pc, target, env), stack = List.hd_tl state.stack in
    let env =
      match state.last_val with
      | None -> env
      | Some v -> Env.add target v env in
    { state with pc; env; stack }

  let print_op state =
    let pc = state.pc in
    let op = Vector.get state.program pc in
    show_pc_opcode pc op

  let is_stopped state =
    let op = current_op state in
    equal_opcode op Stop || state.pc >= Vector.length state.program

  let make program pc = make_state ~program ~pc ()

  let init =
    let program : opcode Vector.ro_vector = Vector.of_list [ Start; Stop ] in
    make_state ~program ~pc:1 ()

  (* TODO
     "Inline" pc/env onto the stack?
     instead of target on the stack, read the value from the call instruction?
     Maybe add a return instruction...
     Fix calling convention
     clean up last_val
     clean up in general
     does this generalize to an abstract state?
  *)
end

(* Type hierarchy: T_Bool < T_Int < T_Str *)
let promote_type t1 t2 =
  let open Expr in
  match (t1, t2) with
  | T_Str, _ | _, T_Str -> T_Str
  | T_Int, _ | _, T_Int -> T_Int
  | T_Bool, _ -> T_Bool

let coerce_data from_ty to_ty data =
  let open Expr in
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
  let str_to_int s =
    Option.bind s (fun s ->
        match Stdlib.int_of_string_opt s with
        | Some i -> Some i
        | None -> raise Common.Coercion_introduces_NA) in

  let coerce unwrap convert wrap = Array.map (unwrap %> convert %> wrap) data in
  match (from_ty, to_ty) with
  | T_Bool, T_Int -> coerce get_bool bool_to_int put_int
  | T_Bool, T_Str -> coerce get_bool bool_to_str put_str
  | T_Int, T_Bool -> coerce get_int int_to_bool put_bool
  | T_Int, T_Str -> coerce get_int int_to_str put_str
  | T_Str, T_Bool -> coerce get_str str_to_bool put_bool
  | T_Str, T_Int -> coerce get_str str_to_int put_int
  | T_Bool, T_Bool | T_Int, T_Int | T_Str, T_Str -> data

let coerce_value to_ty v =
  let open Expr in
  match v with
  | Vector (data, from_ty) -> coerce_data from_ty to_ty data |> vector to_ty
  | Dataframe _ -> raise Not_supported [@coverage off]

let check_type ty v =
  let open Expr in
  match v with
  | Vector (_, vector_ty) as vec ->
      assert (Common.vector_consistent_type vec) ;
      Some (equal_type_tag ty vector_ty) |> put_bool |> vector_of_lit
  | Dataframe _ -> raise Not_supported [@coverage off]

(* Checks that all elements are non-negative or NA.
   0 and NA are allowed for positive subsetting. *)
let is_positive_subsetting = Array.for_all @@ Option.map_or ~default:true (fun x -> x >= 0)

(* Checks that all elements are 0. NA is not allowed. *)
let is_zero_subsetting = Array.for_all @@ Option.map_or ~default:false (fun x -> x = 0)

let contains_na (type t) (a : t option array) = Array.exists (fun x -> Option.is_none x) a

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
  assert (not @@ is_zero_subsetting idxs) ;
  assert (not @@ contains_na idxs) ;
  let idxs = Array.map (fun x -> Option.get_exn_or "internal error" x) idxs in
  let max_idx = Array.fold_left (fun mx i -> max mx i) (Array.length a) idxs in
  let res = extend max_idx t a in
  Array.iter2 (fun i x -> res.(i - 1) <- x) idxs rpl ;
  res

(* Boolean and integer values get coerced for Logical_Not, Unary_Plus, and Unary_Minus;
    strings cannot be coerced. Unary operations on data frames are not supported. *)
let eval_unary op v =
  let open Expr in
  match v with
  | Vector (a, t) as v -> (
      match op with
      | Logical_Not ->
          (* Coerce to boolean, apply logical not *)
          if equal_type_tag t T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Bool |> Array.map (lift bool @@ Option.map not) |> vector T_Bool
      | Unary_Plus ->
          (* Nop for integers, but coerces booleans to integers *)
          if equal_type_tag t T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Int |> vector T_Int
      | Unary_Minus ->
          (* Coerce to integer, apply unary negation *)
          if equal_type_tag t T_Str then raise Invalid_argument_type ;
          a |> coerce_data t T_Int |> Array.map (lift int @@ Option.map ( ~- )) |> vector T_Int
      | As_Logical -> coerce_value T_Bool v
      | As_Integer -> coerce_value T_Int v
      | As_Character -> coerce_value T_Str v
      | Is_Logical -> check_type T_Bool v
      | Is_Integer -> check_type T_Int v
      | Is_Character -> check_type T_Str v
      | Is_NA -> a |> Array.map (is_na %> Option.some %> put_bool) |> vector T_Bool
      | Length -> a |> Array.length |> Option.some |> put_int |> vector_of_lit)
  | Dataframe _ -> raise Not_supported [@coverage off]

let eval_binary op v1 v2 =
  let open Expr in
  let arithmetic_op o =
    let a1, t1 = Pair.(vector_data &&& vector_type) v1 in
    let a2, t2 = Pair.(vector_data &&& vector_type) v2 in

    (* String operands not allowed; but coerce booleans to integers. *)
    if equal_type_tag t1 T_Str || equal_type_tag t2 T_Str then raise Invalid_argument_type ;
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
    let a1, t1 = Pair.(vector_data &&& vector_type) v1 in
    let a2, t2 = Pair.(vector_data &&& vector_type) v2 in

    (* Bools and ints use numeric comparisons, while strings use lexicographic comparisons.
        We need to properly coerce the operands, but also need to handle numeric values and string
        values differently. *)
    match (t1, t2) with
    | T_Str, _ | _, T_Str -> (
        let a1 = a1 |> coerce_data t1 T_Str in
        let a2 = a2 |> coerce_data t2 T_Str in
        let relational f =
          Array.map2 (fun x y -> Option.bind2 f (get_str x) (get_str y) |> put_bool) a1 a2
          |> vector T_Bool in
        match o with
        | Less -> relational (fun x y -> Some (String.compare x y < 0))
        | Less_Equal -> relational (fun x y -> Some (String.compare x y <= 0))
        | Greater -> relational (fun x y -> Some (String.compare x y > 0))
        | Greater_Equal -> relational (fun x y -> Some (String.compare x y >= 0))
        | Equal -> relational (fun x y -> Some (String.compare x y = 0))
        | Not_Equal -> relational (fun x y -> Some (String.compare x y <> 0)))
    | T_Int, _ | _, T_Int | T_Bool, T_Bool -> (
        let a1 = a1 |> coerce_data t1 T_Int in
        let a2 = a2 |> coerce_data t2 T_Int in
        let relational f =
          Array.map2 (fun x y -> Option.bind2 f (get_int x) (get_int y) |> put_bool) a1 a2
          |> vector T_Bool in
        match o with
        | Less -> relational (fun x y -> Some (x < y))
        | Less_Equal -> relational (fun x y -> Some (x <= y))
        | Greater -> relational (fun x y -> Some (x > y))
        | Greater_Equal -> relational (fun x y -> Some (x >= y))
        | Equal -> relational (fun x y -> Some (x = y))
        | Not_Equal -> relational (fun x y -> Some (x <> y))) in

  let logical_op o =
    let a1, t1 = Pair.(vector_data &&& vector_type) v1 in
    let a2, t2 = Pair.(vector_data &&& vector_type) v2 in

    (* String operands not allowed; but coerce integers to booleans. *)
    if equal_type_tag t1 T_Str || equal_type_tag t2 T_Str then raise Invalid_argument_type ;
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
    let e1 = if Array.is_empty a1 then None else get_bool a1.(0) in
    let e2 = if Array.is_empty a2 then None else get_bool a2.(0) in
    match o with
    | And -> and' e1 e2 |> put_bool |> vector_of_lit
    | Or -> or' e1 e2 |> put_bool |> vector_of_lit
    | Elementwise_And -> elementwise and'
    | Elementwise_Or -> elementwise or' in

  (* This needs to be a function, not a constant, because it might raise an exception *)
  let sequence_op () =
    let a1, t1 = Pair.(vector_data &&& vector_type) v1 in
    let a2, t2 = Pair.(vector_data &&& vector_type) v2 in

    if vector_length v1 <> 1 || vector_length v2 <> 1 then raise Expected_scalar ;

    (* Everything gets coerced to integer *)
    let a1 = a1 |> coerce_data t1 T_Int |> Array.map get_int in
    let a2 = a2 |> coerce_data t2 T_Int |> Array.map get_int in

    match (a1.(0), a2.(0)) with
    | Some e1, Some e2 -> vector_of_list int List.(e1 -- e2)
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
  | Vector _, _ | _, Vector _ | Dataframe _, _ -> raise Not_supported [@coverage off]

let eval_subset1 v1 v2 =
  let open Expr in
  match (v1, v2) with
  | Vector (_, _), None -> v1
  | Vector (a1, t1), Some (Vector (a2, t2) as v2) -> (
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
  | Dataframe _, _ -> raise Not_supported [@coverage off]
  | _, Some (Dataframe _) -> raise Invalid_argument_type [@coverage off]

let eval_subset2 v1 v2 =
  let open Expr in
  match (v1, v2) with
  | Vector (a1, _), Vector (a2, t2) -> (
      let n1, n2 = Pair.map_same vector_length (v1, v2) in
      if n2 = 0 || n2 > 1 || equal_type_tag t2 T_Str then raise Invalid_subset_index ;
      let a2 = a2 |> coerce_data t2 T_Int in
      match get_int a2.(0) with
      | Some i when 1 <= i && i <= n1 -> vector_of_lit a1.(i - 1)
      | Some _ | None -> raise Invalid_subset_index)
  | Dataframe _, _ -> raise Not_supported [@coverage off]
  | _, Dataframe _ -> raise Invalid_argument_type [@coverage off]

let eval_subset1_assign v1 idx v3 =
  let open Expr in
  match (v1, idx, v3) with
  | Vector (_, t1), None, Vector (a3, t3) ->
      if vector_length v1 <> vector_length v3 then raise Vector_lengths_do_not_match ;
      let t = promote_type t1 t3 in
      a3 |> coerce_data t3 t |> vector t
  | Vector (a1, t1), Some (Vector (a2, t2) as v2), Vector (a3, t3) -> (
      let n1, n2, n3 = (vector_length v1, vector_length v2, vector_length v3) in
      let t = promote_type t1 t3 in
      let a1, a3 = (coerce_data t1 t a1, coerce_data t3 t a3) in
      match t2 with
      | T_Bool ->
          let a2 = a2 |> Array.map get_bool in
          if contains_na a2 then raise Invalid_subset_index ;
          if n1 <> n2 then raise Vector_lengths_do_not_match ;
          let a2 = a2 |> bool_to_pos_vector in
          let n2 = Array.length a2 in
          if n2 <> n3 then raise Invalid_subset_replacement ;
          update_at_pos t a1 a2 a3 |> vector t
      | T_Int ->
          let a2 = a2 |> Array.map get_int in
          if contains_na a2 || (not @@ is_positive_subsetting a2) then raise Invalid_subset_index ;
          if is_zero_subsetting a2 then vector t a1
          else
            let a2 = a2 |> Array.filter (fun x -> not @@ Option.equal CCEqual.int x (Some 0)) in
            let n2 = Array.length a2 in
            if n2 <> n3 then raise Invalid_subset_replacement ;
            update_at_pos t a1 a2 a3 |> vector t
      | T_Str -> raise Invalid_argument_type)
  | Dataframe _, _, _ -> raise Not_supported [@coverage off]
  | _, Some (Dataframe _), _ | _, _, Dataframe _ -> raise Invalid_argument_type

let eval_subset2_assign v1 idx v3 =
  let open Expr in
  match (v1, idx, v3) with
  | Vector (a1, t1), (Vector (a2, t2) as v2), (Vector (a3, t3) as v3) -> (
      let n2, n3 = Pair.map_same vector_length (v2, v3) in
      if n2 <> 1 || equal_type_tag t2 T_Str then raise Invalid_subset_index ;
      if n3 <> 1 then raise Invalid_subset_replacement ;
      let t = promote_type t1 t3 in
      let a1 = a1 |> coerce_data t1 t in
      let a2 = a2 |> coerce_data t2 T_Int in
      let a3 = a3 |> coerce_data t3 t in
      match get_int a2.(0) with
      | Some i when 1 <= i ->
          let a1 = extend i t a1 in
          a1.(i - 1) <- a3.(0) ;
          vector t a1
      | Some _ | None -> raise Invalid_subset_index)
  | Dataframe _, _, _ -> raise Not_supported [@coverage off]
  | _, Dataframe _, _ | _, _, Dataframe _ -> raise Invalid_argument_type

let eval_builtin builtin args =
  match (builtin, args) with
  | Unary op, [ v1 ] -> eval_unary op v1
  | Binary op, [ v1; v2 ] -> eval_binary op v1 v2
  | Combine, values ->
      (* Get the least upper bound of all types
          Then coerce all vectors to that type, extract, and concatenate the data *)
      let ty = values |> List.map vector_type |> List.fold_left promote_type E.T_Bool in
      let data = values |> List.map (coerce_value ty) |> List.map vector_data |> Array.concat in
      vector ty data
  | Input, [ v1 ] -> v1
  | Subset1, [ v1 ] -> eval_subset1 v1 None
  | Subset1, [ v1; idx ] -> eval_subset1 v1 (Some idx)
  | Subset2, [ v1; idx ] -> eval_subset2 v1 idx
  | Subset1_Assign, [ v1; v3 ] -> eval_subset1_assign v1 None v3
  | Subset1_Assign, [ v1; idx; v3 ] -> eval_subset1_assign v1 (Some idx) v3
  | Subset2_Assign, [ v1; idx; v3 ] -> eval_subset2_assign v1 idx v3
  | (Unary _ | Binary _ | Input | Subset1 | Subset2 | Subset1_Assign | Subset2_Assign), _ ->
      (* These are the cases for when we pass the wrong number of arguments. *)
      raise Internal_error

let eval_branch = function
  | E.Vector _ as v1 -> (
      if vector_length v1 <> 1 then raise Expected_scalar ;
      let a1 = v1 |> coerce_value T_Bool |> vector_data |> Array.map get_bool in
      match a1.(0) with
      | None -> raise Missing_value_need_true_false
      | Some b -> b)
  | E.Dataframe _ -> raise Invalid_argument_type

let eval ?(debug = false) state =
  let eval_se = function
    | E.Lit l -> vector_of_lit l
    | E.Var x -> State.lookup x state in

  let op = State.current_op state in
  if debug then Printf.eprintf "%s\n%!" (State.print_op state) ;

  match op with
  | Copy (x, se) ->
      let v = eval_se se in
      state |> State.update x v |> State.advance_pc Next
  | Call { target; fn_pc; params; args_se; _ } ->
      let args = List.map eval_se args_se in
      State.push target fn_pc params args state
  | Builtin (x, builtin, ses) ->
      let args = List.map eval_se ses in
      let v = eval_builtin builtin args in

      (* Complex assignment is tricky, the "last value" is the RHS, not the LHS after assignment.
         In most cases, the two are the same. But for subset assignment, coercions may happen,
         so we need to make sure we return the RHS, which is the last argument. *)
      let last_val =
        match[@warning "-4"] builtin with
        | Subset1_Assign | Subset2_Assign -> Some (List.hd @@ List.rev args)
        | _ -> Some v in

      state |> State.update x v ~last_val:(Some last_val) |> State.advance_pc Next
  | Exit _ -> State.pop state
  | Jump newpc -> State.advance_pc (Jump newpc) state
  | Branch (se, true_pc) ->
      let cond = eval_se se in
      if eval_branch cond then State.advance_pc (Jump true_pc) state
      else State.advance_pc Next state
  | Print se ->
      eval_se se |> E.show_val |> Stdlib.print_endline ;
      state |> State.clear_last_val |> State.advance_pc Next
  | Nop | Start | Stop | Entry _ | Comment _ -> State.advance_pc Next state

let rec eval_continuous ?(debug = false) state =
  if State.is_stopped state then state
  else
    let state' = eval ~debug state in
    (eval_continuous ~debug [@tailcall]) state'

let run ?(debug = false) (program, pc) =
  let state = eval_continuous ~debug @@ State.make program pc in
  state.last_val

let run_str ?(debug = false) str = str |> Parser.parse |> Compile.compile |> run ~debug

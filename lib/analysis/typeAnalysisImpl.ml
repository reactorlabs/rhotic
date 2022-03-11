open Containers
open Util
open Opcode

(* This is a static analysis that represents each program value by its type. *)

module E = Expr
module Env = Map.Make (E.Identifier)

module AValue = struct
  (* This is the lattice that represents program values.
      Note that the abstract step function computes on abstract _states_.
      For this analysis, an abstract state is an abstract environment,
      mapping identifiers to abstract values.

      The lattice is defined as:

            Top
          /  |  \
       Bool Int Str
          \  |  /
            Bot
  *)

  type t =
    | Top [@printer fun fmt _ -> fprintf fmt "⊤"]
    | Bool [@printer fun fmt _ -> fprintf fmt "B"]
    | Int [@printer fun fmt _ -> fprintf fmt "I"]
    | Str [@printer fun fmt _ -> fprintf fmt "S"]
    | Bot [@printer fun fmt _ -> fprintf fmt "⊥"]
  [@@deriving eq, show { with_path = false }]

  let leq x y =
    match (x, y) with
    | Bot, _ | _, Top -> true
    | Bool, Bool | Int, Int | Str, Str -> true
    | _, Bot | Top, _ -> false
    | Bool, (Int | Str) | Int, (Bool | Str) | Str, (Bool | Int) -> false

  let merge x y =
    match (x, y) with
    | Top, _ | _, Top -> Top
    | Bot, z | z, Bot -> z
    | Str, Str | Int, Int | Bool, Bool -> x
    | Bool, (Int | Str) | Int, (Bool | Str) | Str, (Bool | Int) -> Top

  let promote_type x y =
    match (x, y) with
    | Top, _ | _, Top -> Top
    | Str, _ | _, Str -> Str
    | Int, _ | _, Int -> Int
    | Bool, _ | _, Bool -> Bool
    | Bot, Bot -> Bot

  let abstract = function
    | E.Bool _ | E.NA_bool -> Bool
    | E.Int _ | E.NA_int -> Int
    | E.Str _ | E.NA_str -> Str
end

(* Only last_val and env are part of the abstract state.
   The other fields are needed for debugging and computation. *)
type astate =
  { pc : pc
  ; op : opcode
  ; cur_fun : E.Identifier.t
  ; succs : pc list
  ; ret_vars : E.Identifier.t list
  ; last_val : AValue.t
  ; env : AValue.t Env.t
  }

let init pc op cur_fun succs ret_vars =
  { pc; op; cur_fun; succs; ret_vars; last_val = AValue.Bot; env = Env.empty }

let show_op state = show_pc_opcode state.pc state.op

let show state =
  let last_val_str = Printf.sprintf "res: %s" @@ AValue.show state.last_val in
  let env_str =
    let prefix = state.cur_fun ^ "$$" in
    let env_list =
      state.env |> Env.bindings
      |> List.filter_map (fun (x, v) ->
             x |> String.chop_prefix ~pre:prefix |> Option.map (fun x -> (x, v)))
      |> List.filter (fun (x, _) -> not @@ String.contains x '$') in
    if List.is_empty env_list then ""
    else
      List.to_string ~start:" | env: " ~sep:", "
        (fun (x, v) -> Printf.sprintf "%s ↦ %s" x (AValue.show v))
        env_list in
  Printf.sprintf "%s%s" last_val_str env_str

(* x leq y
    <=>
      x.last_val leq y.last_val
      /\ x.env leq y.env

    Comparing two maps mx and my:
      forall k in dom(mx): my(k) exists and mx(k) leq my(k) *)
let leq x y =
  let last_val = AValue.leq x.last_val y.last_val in
  let env =
    Env.bindings x.env |> List.map Stdlib.fst
    |> List.for_all (fun id ->
           Env.mem id y.env && AValue.leq (Env.find id x.env) (Env.find id y.env)) in
  last_val && env

(* x merge y
    = { x.last_val merge y.last_val
      ; x.env merge y.env }

    Merging two maps mx and my:
      mx(k) merge my(k)
    where an undefined mapping is treated as Bot *)
let merge x y =
  (* TODO: We should only be merging states for the same pc, but the assert fails. *)
  (* assert ( *)
  (*   equal_pc x.pc y.pc && equal_opcode x.op y.op *)
  (*   && E.equal_identifier x.cur_fun y.cur_fun *)
  (*   && List.same_elts compare_pc x.succs y.succs *)
  (*   && List.same_elts E.compare_identifier x.ret_vars y.ret_vars) ; *)
  let last_val = AValue.merge x.last_val y.last_val in
  let env = Env.union (fun _ vx vy -> Some (AValue.merge vx vy)) x.env y.env in
  { x with last_val; env }

let successors state = state.succs

let step state =
  let open Expr in
  let lookup x state =
    let id = state.cur_fun ^ "$$" ^ x in
    Env.get_or id state.env ~default:AValue.Bot in

  let update ?(strong = false) x v state =
    let id = state.cur_fun ^ "$$" ^ x in
    let old = Env.get_or id state.env ~default:AValue.Bot in
    let v' = if strong then v else AValue.merge old v in
    { state with env = Env.add id v' state.env; last_val = v' } in

  let push fn params args state =
    let env =
      List.fold_left2
        (fun e x v ->
          let id = fn ^ "$$" ^ x in
          let old = Env.get_or id e ~default:AValue.Bot in
          let v' = AValue.merge old v in
          Env.add id v' e)
        state.env params args in
    { state with env } in

  let pop state =
    let ids = state.ret_vars in
    let env =
      List.fold_left
        (fun e id ->
          let old = Env.get_or id e ~default:AValue.Bot in
          let v' = AValue.merge old state.last_val in
          Env.add id v' e)
        state.env ids in
    { state with env } in

  let eval_se = function
    | Lit l -> AValue.abstract l
    | Var x -> lookup x state in

  let eval_builtin builtin args =
    let eval_unary op v =
      match op with
      | As_Logical | Is_Logical | Is_Integer | Is_Character | Is_NA ->
          if AValue.equal AValue.Bot v then AValue.Bot else AValue.Bool
      | As_Integer | Length -> if AValue.equal AValue.Bot v then AValue.Bot else AValue.Int
      | As_Character -> if AValue.equal AValue.Bot v then AValue.Bot else AValue.Str
      | Logical_Not -> (
          match v with
          | AValue.Top -> AValue.Top
          | AValue.Int | AValue.Bool -> AValue.Bool
          | AValue.Bot | AValue.Str -> AValue.Bot)
      | Unary_Plus | Unary_Minus -> (
          match v with
          | AValue.Top -> AValue.Top
          | AValue.Int | AValue.Bool -> AValue.Int
          | AValue.Bot | AValue.Str -> AValue.Bot) in

    let eval_binary op v1 v2 =
      match op with
      | Arithmetic _ -> (
          match (v1, v2) with
          | AValue.Top, _ | _, AValue.Top -> AValue.Top
          | (AValue.Int | AValue.Bool), (AValue.Int | AValue.Bool) -> AValue.Int
          | (AValue.Bot | AValue.Str), _ | _, (AValue.Bot | AValue.Str) -> AValue.Bot)
      | Relational _ -> (
          match (v1, v2) with
          | AValue.Top, _ | _, AValue.Top -> AValue.Top
          | (AValue.Str | AValue.Int | AValue.Bool), (AValue.Str | AValue.Int | AValue.Bool) ->
              AValue.Bool
          | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot)
      | Logical _ -> (
          match (v1, v2) with
          | AValue.Top, _ | _, AValue.Top -> AValue.Top
          | (AValue.Int | AValue.Bool), (AValue.Int | AValue.Bool) -> AValue.Bool
          | (AValue.Bot | AValue.Str), _ | _, (AValue.Bot | AValue.Str) -> AValue.Bot)
      | Seq ->
          if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v2 then AValue.Bot
          else AValue.Int in

    match (builtin, args) with
    | Unary op, [ v1 ] -> eval_unary op v1
    | Binary op, [ v1; v2 ] -> eval_binary op v1 v2
    | Combine, values ->
        if List.mem AValue.Bot values then AValue.Bot
        else List.fold_left AValue.promote_type AValue.Bot values
    | Input, [ _ ] -> AValue.Top
    | Subset1, [ v1 ] -> v1
    | (Subset1 | Subset2), [ v1; idx ] -> (
        match idx with
        | AValue.Top -> AValue.Top
        | AValue.Int | AValue.Bool -> v1
        | AValue.Bot | AValue.Str -> AValue.Bot)
    | Subset1_Assign, [ v1; v3 ] ->
        if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v3 then AValue.Bot
        else AValue.promote_type v1 v3
    | (Subset1_Assign | Subset2_Assign), [ v1; idx; v3 ] -> (
        match idx with
        | AValue.Top -> AValue.Top
        | AValue.Int | AValue.Bool ->
            if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v3 then AValue.Bot
            else AValue.merge v1 v3
        | AValue.Bot | AValue.Str -> AValue.Bot)
    | (Unary _ | Binary _ | Input | Subset1 | Subset2 | Subset1_Assign | Subset2_Assign), _ ->
        (* These are the cases for when we pass the wrong number of arguments. *)
        assert false in

  match state.op with
  | Copy (x, se) ->
      let v = eval_se se in
      update ~strong:true x v state
  | Call { fn; params; args_se; _ } ->
      let args = List.map eval_se args_se in
      push fn params args state
  | Builtin (x, builtin, ses) ->
      let args = List.map eval_se ses in
      let v = eval_builtin builtin args in

      (* Complex assignment is tricky, the "last value" is the RHS.
         Strong update unless we have a non-empty subset assignment. *)
      let last_val, strong =
        match[@warning "-4"] builtin with
        | Subset1_Assign | Subset2_Assign ->
            let strong' = if List.length ses = 2 then true else false in
            (List.hd @@ List.rev args, strong')
        | _ -> (v, true) in

      let state' = update ~strong x v state in
      { state' with last_val }
  | Exit _ -> pop state
  | Jump _ | Branch _ | Nop | Start | Stop | Print _ | Entry _ | Comment _ -> state

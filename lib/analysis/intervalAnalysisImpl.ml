open Containers
open Util
open Opcode

(* This is a static analysis that computes intervals, representing the length of program values *)

module E = Expr
module Env = E.Env

module AValue = struct
  (* This is the lattice that represents program values.
     Note that the abstract step function computes on abstract _states_.
     For this analysis, an abstract state is an abstract environment,
     mapping identifiers to abstract values.

     The lattice is defined as:

            Top
             |
           [a,b]
             |
            Bot

     where [a,b] leq [c,d] iff c <= a and d >= b.

     Note that this lattice has infinite height.
  *)

  type t =
    | Top [@printer fun fmt _ -> fprintf fmt "⊤"]
    | Interval of int * int [@printer fun fmt (a, b) -> fprintf fmt "[%d,%d]" a b]
    | Bot [@printer fun fmt _ -> fprintf fmt "⊥"]
  [@@deriving eq, show { with_path = false }]

  let leq x y =
    match (x, y) with
    | Bot, _ | _, Top -> true
    | Interval (a, b), Interval (c, d) -> c <= a && d >= b
    | _, Bot | Top, _ -> false

  let merge x y =
    match (x, y) with
    | Top, _ | _, Top -> Top
    | Interval (a, b), Interval (c, d) -> Interval (Stdlib.min a c, Stdlib.max b d)
    | Bot, z | z, Bot -> z
end

(* Only last_val and env are part of the abstract state.
   The other fields are for bookkeeping. *)
type astate =
  { pc : pc
  ; op : opcode
  ; last_val : AValue.t [@default AValue.Bot]
  ; env : AValue.t Env.t [@default Env.empty]
  }
[@@deriving make]

let init pc op = make_astate ~pc ~op ()

let show_op state = show_pc_opcode state.pc state.op

let show state =
  let last_val_str = Printf.sprintf "res: %s" @@ AValue.show state.last_val in
  let env_str =
    let env_list =
      state.env |> Env.bindings |> List.filter (fun (x, _) -> not @@ String.contains x '$') in
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
    Env.bindings x.env
    |> List.for_all (fun (id, _) ->
           Env.mem id y.env && AValue.leq (Env.find id x.env) (Env.find id y.env)) in
  last_val && env

(* x merge y
    = { x.last_val merge y.last_val
      ; x.env merge y.env }

    Merging two maps mx and my:
      mx(k) merge my(k)
    where an undefined mapping is treated as Bot *)
let merge x y =
  let last_val = AValue.merge x.last_val y.last_val in
  let env = Env.union (fun _ vx vy -> Some (AValue.merge vx vy)) x.env y.env in
  { x with last_val; env }

let lookup x state = Env.get_or x state.env ~default:AValue.Bot

let update ?(strong = false) x v state =
  let old = lookup x state in
  let v' = if strong then v else AValue.merge old v in
  { state with env = Env.add x v' state.env; last_val = v' }

let eval_se state = function
  | E.Lit _ -> AValue.Interval (1, 1)
  | E.Var x -> lookup x state

let call params args_se state =
  let args = List.map (eval_se state) args_se in
  let env =
    List.fold_left2
      (fun e x v ->
        let old = Env.get_or x e ~default:AValue.Bot in
        let v' = AValue.merge old v in
        Env.add x v' e)
      state.env params args in
  { state with env }

let return targets state =
  let env =
    List.fold_left
      (fun e x ->
        let old = Env.get_or x e ~default:AValue.Bot in
        let v' = AValue.merge old state.last_val in
        Env.add x v' e)
      state.env targets in
  { state with env }

let step state =
  let open Expr in
  let eval_builtin builtin args ses =
    let eval_unary op v =
      match op with
      | Logical_Not | Unary_Plus | Unary_Minus | As_Logical | As_Integer | As_Character -> v
      | Is_Logical | Is_Integer | Is_Character | Is_NA | Length ->
          if AValue.equal AValue.Bot v then AValue.Bot else AValue.Interval (1, 1) in

    let eval_binary op v1 v2 =
      match op with
      | Arithmetic _ | Relational _ | Logical (Elementwise_And | Elementwise_Or) -> (
          match (v1, v2) with
          | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot
          | (AValue.Top | AValue.Interval _), (AValue.Top | AValue.Interval _) -> AValue.merge v1 v2
          )
      | Logical (And | Or) -> (
          match (v1, v2) with
          | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot
          | (AValue.Top | AValue.Interval _), (AValue.Top | AValue.Interval _) ->
              AValue.Interval (1, 1))
      | Seq -> (
          (* We have to look at the unevaluated arguments to the binary operation.
             If they're literals, we can use their values to construct an interval.
             Otherwise, we can't do anything and return Top. *)
          let coerce = function
            | E.Bool b -> Bool.to_int b
            | E.Int i -> i
            | E.Str s -> Int.of_string_exn s
            | E.NA_bool | E.NA_int | E.NA_str -> raise Common.Coercion_introduces_NA in

          match[@warning "-4"] ses with
          | [ E.Lit l1; E.Lit l2 ] -> (
              try
                let i1, i2 = (coerce l1, coerce l2) in
                let l = Stdlib.abs (i2 - i1) + 1 in
                AValue.Interval (l, l)
              with _ -> AValue.Bot)
          | [ _; _ ] -> AValue.Top
          | _ -> assert false) in

    match (builtin, args) with
    | Unary op, [ v1 ] -> eval_unary op v1
    | Binary op, [ v1; v2 ] -> eval_binary op v1 v2
    | Combine, values ->
        List.fold_left
          (fun acc interval ->
            match (acc, interval) with
            | AValue.Top, _ | _, AValue.Top -> AValue.Top
            | AValue.Interval (a, b), AValue.Interval (c, d) -> AValue.Interval (a + c, b + d)
            | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot)
          (AValue.Interval (0, 0))
          values
    | Input, [ _ ] -> AValue.Top
    | Subset1, [ v1 ] -> v1
    | Subset1, [ _; idx ] -> (
        match idx with
        | AValue.Top -> AValue.Top
        | AValue.Interval (_, i) -> AValue.Interval (0, i)
        | AValue.Bot -> AValue.Bot)
    | Subset2, [ _; idx ] -> (
        match idx with
        | AValue.Top -> AValue.Top
        | AValue.Interval _ -> AValue.Interval (1, 1)
        | AValue.Bot -> AValue.Bot)
    | Subset1_Assign, [ v1; v3 ] ->
        if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v3 then AValue.Bot else v3
    | (Subset1_Assign | Subset2_Assign), [ v1; idx; v3 ] ->
        if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot idx || AValue.equal AValue.Bot v3
        then AValue.Bot
        else v3
    | (Unary _ | Binary _ | Input | Subset1 | Subset2 | Subset1_Assign | Subset2_Assign), _ ->
        (* These are the cases for when we pass the wrong number of arguments. *)
        assert false in

  match state.op with
  | Copy (x, se) ->
      let v = eval_se state se in
      update ~strong:true x v state
  | Builtin (x, builtin, ses) ->
      let args = List.map (eval_se state) ses in
      let v = eval_builtin builtin args ses in

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
  | Call _ | Exit _ -> assert false
  | Jump _ | Branch _ | Nop | Start | Stop | Print _ | Entry _ | Comment _ -> state

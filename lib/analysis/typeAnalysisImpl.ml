open Containers
open Util
open Opcode

(* This is a static analysis that represents each program value by its type. *)

module E = Expr
module Env = E.Env

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

  let abstract_lit = function
    | E.Bool _ | E.NA_bool -> Bool
    | E.Int _ | E.NA_int -> Int
    | E.Str _ | E.NA_str -> Str

  let abstract_val v =
    match Common.vector_type v with
    | T_Bool -> Bool
    | T_Int -> Int
    | T_Str -> Str
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

let show_op astate = show_pc_opcode astate.pc astate.op

let show astate =
  let last_val_str = Printf.sprintf "res: %s" @@ AValue.show astate.last_val in
  let env_str =
    let env_list =
      astate.env |> Env.bindings |> List.filter (fun (x, _) -> not @@ String.contains x '$') in
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

let lookup x astate = Env.get_or x astate.env ~default:AValue.Bot

let update ?(strong = false) x v astate =
  let old = lookup x astate in
  let v' = if strong then v else AValue.merge old v in
  { astate with env = Env.add x v' astate.env; last_val = v' }

let eval_se astate = function
  | E.Lit l -> AValue.abstract_lit l
  | E.Var x -> lookup x astate

let call params args_se ?cstate:_ astate =
  let args = List.map (eval_se astate) args_se in
  let env =
    List.fold_left2
      (fun e x v ->
        let old = Env.get_or x e ~default:AValue.Bot in
        let v' = AValue.merge old v in
        Env.add x v' e)
      astate.env params args in
  { astate with env }

let return targets ?cstate:_ astate =
  let env =
    List.fold_left
      (fun e x ->
        let old = Env.get_or x e ~default:AValue.Bot in
        let v' = AValue.merge old astate.last_val in
        Env.add x v' e)
      astate.env targets in
  { astate with env }

let step ?cstate:_ astate =
  let open Expr in
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

  match astate.op with
  | Copy (x, se) ->
      let v = eval_se astate se in
      update ~strong:true x v astate
  | Builtin (x, builtin, ses) ->
      let args = List.map (eval_se astate) ses in
      let v = eval_builtin builtin args in

      (* Complex assignment is tricky, the "last value" is the RHS.
         Strong update unless we have a non-empty subset assignment. *)
      let last_val, strong =
        match[@warning "-4"] builtin with
        | Subset1_Assign | Subset2_Assign ->
            let strong' = if List.length ses = 2 then true else false in
            (List.hd @@ List.rev args, strong')
        | _ -> (v, true) in

      let astate' = update ~strong x v astate in
      { astate' with last_val }
  | Call _ | Exit _ -> assert false
  | Jump _ | Branch _ | Nop | Start | Stop | Print _ | Entry _ | Comment _ -> astate

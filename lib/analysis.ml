open Containers
open Common
open Util
open Opcode

module E = Expr

(*
TODO:
  - clean up interface/modules
  - plug in another analysis, e.g. vector length? backwards analysis?
*)

(* TODO: generate CFG and save it, useful for reverse analysis *)
let sucessors program pc =
  match Vector.get program pc with
  | Stop -> []
  | Copy _ | Builtin _ | Print _ | Nop | Start | Entry _ | Comment _ -> [ pc + 1 ]
  | Jump pc' -> [ pc' ]
  | Branch (_, pc') -> [ pc + 1; pc' ]
  | Call { fn_pc; _ } -> [ fn_pc ]
  | Exit id ->
      (* Need to find all sites that call f; sucessor is the nop after the callsite *)
      program
      |> Vector.foldi
           (fun acc i op ->
             match[@warning "-4"] op with
             | Call { fn; _ } when E.Identifier.equal fn id -> (i + 1) :: acc
             | _ -> acc)
           List.empty

(* TODO: cache this somewhere *)
(* From the current pc, advance until we find an "Exit" or "Stop" instruction,
    which indicates the name of the current function we're in. *)
exception Found_function of E.identifier
let current_function program pc =
  let n = Vector.length program in
  try
    for i = pc to n - 1 do
      match[@warning "-4"] Vector.get program i with
      | Exit id -> raise @@ Found_function id
      | Stop -> raise @@ Found_function ""
      | _ -> ()
    done ;
    raise Internal_error
  with Found_function id -> id

(*
    - merge (union or intersection?)
    - F (forward or backward edges of CFG)
    - E (entry or exit nodes)
      - entry_pc or exit_pc
    - iota (initial or final analysis info)
    - f (abstract step, transfer functions)
    - lattice
      - elements
      - leq
      - merge (lub and glb?)
   *)

(* can probably combine the AValue, AState, and Analysis modules *)

module AValue = struct
  (*
      Top
       |
      Str
       |
      Int
       |
      Bool
       |
      Bot

    Type declaration is "backwards" to make `compare` work as expected.
  *)
  type t =
    | Bot [@printer fun fmt _ -> fprintf fmt "⊥"]
    | Bool [@printer fun fmt _ -> fprintf fmt "B"]
    | Int [@printer fun fmt _ -> fprintf fmt "I"]
    | Str [@printer fun fmt _ -> fprintf fmt "S"]
    | Top [@printer fun fmt _ -> fprintf fmt "⊤"]
  [@@deriving eq, ord, show { with_path = false }]

  let leq x y = compare x y <= 0

  let merge x y =
    match (x, y) with
    | Top, _ | _, Top -> Top
    | Bot, z | z, Bot -> z
    | Str, Str | Int, Int | Bool, Bool -> x
    | Str, (Int | Bool) | (Int | Bool), Str -> Str
    | Int, Bool | Bool, Int -> Int

  let abstract = function
    | E.Bool _ | E.NA_bool -> Bool
    | E.Int _ | E.NA_int -> Int
    | E.Str _ | E.NA_str -> Str
end

module AState = struct
  module Env = Map.Make (E.Identifier)
  type environment = AValue.t Env.t
  type state =
    { program : opcode Vector.ro_vector (* for bookkeeping *)
    ; pc : pc (* pc is just for debugging *)
    ; last_val : AValue.t [@default AValue.Bot]
    ; env : environment [@default Env.empty]
    }
  [@@deriving make]

  let print_op state =
    let pc = state.pc in
    let op = Vector.get state.program pc in
    show_pc_opcode pc op

  let print state =
    let last_val_str = Printf.sprintf "res: %s" @@ AValue.show state.last_val in
    let env_str =
      let cur_fun = current_function state.program state.pc in
      let prefix = cur_fun ^ "$$" in
      let env_list =
        state.env |> Env.bindings
        |> List.filter_map (fun (x, v) ->
               String.chop_prefix ~pre:prefix x |> Option.map (fun x -> (x, v)))
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

  (* x lub y
     = { x.last_val lub y.last_val
       ; x.env lub y.env }

     Merging two maps mx and my:
       mx(k) merge my(k)
     where an undefined mapping is treated as Bot *)
  let merge pc x y =
    let last_val = AValue.merge x.last_val y.last_val in
    let env = Env.union (fun _ vx vy -> Some (AValue.merge vx vy)) x.env y.env in
    make_state ~program:x.program ~pc ~last_val ~env ()

  let init program pc = make_state ~program ~pc ()

  let set_last_val last_val state = { state with last_val }

  let lookup x state =
    let cur_fun = current_function state.program state.pc in
    let id = cur_fun ^ "$$" ^ x in
    Env.get_or id state.env ~default:AValue.Bot

  let update ?(strong = false) x v state =
    let fn = current_function state.program state.pc in
    let id = fn ^ "$$" ^ x in
    let old = Env.get_or id state.env ~default:AValue.Bot in
    let v' = if strong then v else AValue.merge old v in
    { state with env = Env.add id v' state.env; last_val = v' }

  let push fn params args state =
    let env =
      List.fold_left2
        (fun e x v ->
          let id = fn ^ "$$" ^ x in
          let old = Env.get_or id e ~default:AValue.Bot in
          let v' = AValue.merge old v in
          Env.add id v' e)
        state.env params args in
    { state with env }

  let pop state =
    let program, pc = (state.program, state.pc) in
    let return_sites = sucessors program pc in
    let ids =
      List.map
        (fun pc ->
          let[@warning "-8"] (Call { target; _ }) = Vector.get program (pc - 1) in
          let fn = current_function program pc in
          fn ^ "$$" ^ target)
        return_sites in
    let env =
      List.fold_left
        (fun e id ->
          let old = Env.get_or id e ~default:AValue.Bot in
          let v' = AValue.merge old state.last_val in
          Env.add id v' e)
        state.env ids in
    { state with env }
end

module Analysis = struct
  let step (state : AState.state) =
    let open Expr in
    let eval_se = function
      | Lit l -> AValue.abstract l
      | Var x -> AState.lookup x state in

    let eval_builtin builtin args =
      let eval_unary op v =
        match op with
        | As_Logical | Is_Logical | Is_Integer | Is_Character | Is_NA ->
            if AValue.equal AValue.Bot v then AValue.Bot else AValue.Bool
        | As_Integer | Length -> if AValue.equal AValue.Bot v then AValue.Bot else AValue.Int
        | As_Character -> if AValue.equal AValue.Bot v then AValue.Bot else AValue.Str
        | Logical_Not -> (
            match v with
            | AValue.Top | AValue.Str -> AValue.Top
            | AValue.Int | AValue.Bool -> AValue.Bool
            | AValue.Bot -> AValue.Bot)
        | Unary_Plus | Unary_Minus -> (
            match v with
            | AValue.Top | AValue.Str -> AValue.Top
            | AValue.Int | AValue.Bool -> AValue.Int
            | AValue.Bot -> AValue.Bot) in

      let eval_binary op v1 v2 =
        match op with
        | Arithmetic _ -> (
            match (v1, v2) with
            | (AValue.Top | AValue.Str), _ | _, (AValue.Top | AValue.Str) -> AValue.Top
            | (AValue.Int | AValue.Bool), (AValue.Int | AValue.Bool) -> AValue.Int
            | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot)
        | Relational _ -> (
            match (v1, v2) with
            | AValue.Top, _ | _, AValue.Top -> AValue.Top
            | (AValue.Str | AValue.Int | AValue.Bool), (AValue.Str | AValue.Int | AValue.Bool) ->
                AValue.Bool
            | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot)
        | Logical _ -> (
            match (v1, v2) with
            | (AValue.Top | AValue.Str), _ | _, (AValue.Top | AValue.Str) -> AValue.Top
            | (AValue.Int | AValue.Bool), (AValue.Int | AValue.Bool) -> AValue.Bool
            | AValue.Bot, _ | _, AValue.Bot -> AValue.Bot)
        | Seq ->
            if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v2 then AValue.Bot
            else AValue.Int in

      match (builtin, args) with
      | Unary op, [ v1 ] -> eval_unary op v1
      | Binary op, [ v1; v2 ] -> eval_binary op v1 v2
      | Combine, values ->
          if List.mem AValue.Bot values then AValue.Bot
          else List.fold_left (fun x y -> AValue.merge x y) AValue.Bot values
      | Input, [ _ ] -> AValue.Top
      | Subset1, [ v1 ] -> v1
      | (Subset1 | Subset2), [ v1; idx ] -> (
          match idx with
          | AValue.Top | AValue.Str -> AValue.Top
          | AValue.Int | AValue.Bool -> v1
          | AValue.Bot -> AValue.Bot)
      | Subset1_Assign, [ v1; v3 ] ->
          if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v3 then AValue.Bot
          else AValue.merge v1 v3
      | (Subset1_Assign | Subset2_Assign), [ v1; idx; v3 ] -> (
          match idx with
          | AValue.Top | AValue.Str -> AValue.Top
          | AValue.Int | AValue.Bool ->
              if AValue.equal AValue.Bot v1 || AValue.equal AValue.Bot v3 then AValue.Bot
              else AValue.merge v1 v3
          | AValue.Bot -> AValue.Bot)
      | (Unary _ | Binary _ | Input | Subset1 | Subset2 | Subset1_Assign | Subset2_Assign), _ ->
          (* These are the cases for when we pass the wrong number of arguments. *)
          raise Internal_error in

    match Vector.get state.program state.pc with
    | Copy (x, se) ->
        let v = eval_se se in
        AState.update ~strong:true x v state
    | Call { fn; params; args_se; _ } ->
        let args = List.map eval_se args_se in
        AState.push fn params args state
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

        state |> AState.update ~strong x v |> AState.set_last_val last_val
    | Exit _ -> AState.pop state
    | Jump _ | Branch _ | Nop | Start | Stop | Print _ | Entry _ | Comment _ -> state
end

let analyze ?(debug = false) program entry_pc =
  (* Initialize the array of states and the worklist. *)
  let analysis = Vector.init (Vector.length program) (fun pc -> AState.init program pc) in
  let worklist = Stack.create () in
  Stack.push entry_pc worklist ;

  (* Create a "seen" list to make sure everything is visited at least once. *)
  let seen = Array.make (Vector.length program) false in

  while not @@ Stack.is_empty worklist do
    (if debug then
     let worklist_str = Stack.to_list worklist |> List.to_string Int.to_string in
     Printf.eprintf "\t  Worklist: %s\n%!" worklist_str) ;

    (* Pop from worklist, mark seen, and step the abstract state *)
    let pc = Stack.pop worklist in
    let state = Vector.get analysis pc in
    if debug then Printf.eprintf "%s\n%!" @@ AState.print_op state ;

    seen.(pc) <- true ;
    let state' = Analysis.step state in
    if debug then Printf.eprintf "\t  After  %d:\t%s\n%!" pc (AState.print state') ;

    (* Compare the new state to the successors' states *)
    sucessors program pc
    |> List.iter (fun pc' ->
           let succ_state = Vector.get analysis pc' in
           if debug then Printf.eprintf "\t  Before %d:\t%s\n%!" pc' (AState.print succ_state) ;

           (* No new information, but push to worklist if we haven't visited pc' before *)
           if AState.leq state' succ_state then (if not seen.(pc') then Stack.push pc' worklist)
           else
             (* New information: merge/update the successor state, and add successor to worklist *)
             let merged = AState.merge pc' succ_state state' in
             Vector.set analysis pc' merged ;
             if debug then Printf.eprintf "\t  Merged %d:\t%s\n%!" pc' (AState.print merged) ;
             Stack.push pc' worklist)
  done ;

  let analysis' = Vector.map (fun s -> Analysis.step s) analysis in

  if debug then (
    Printf.eprintf "\nAnalysis output:\n" ;
    Vector.iteri
      (fun pc op ->
        let state, state' = (Vector.get analysis pc, Vector.get analysis' pc) in
        Printf.eprintf "\t  Before %d:\t%s\n%!" pc (AState.print state) ;
        Printf.eprintf "%s\n%!" @@ Opcode.show_pc_opcode pc op ;
        Printf.eprintf "\t  After  %d:\t%s\n%!" pc (AState.print state'))
      program) ;

  (Vector.freeze analysis, Vector.freeze analysis')

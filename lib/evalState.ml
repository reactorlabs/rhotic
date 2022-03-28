open Containers
open Util
open Common
open Opcode

module E = Expr
module Env = E.Env

type environment = E.value Env.t

(* callsite pc, target variable for result, function id, caller environment *)
type frame = pc * E.identifier * E.identifier * environment

type advance_type =
  | Next
  | Jump of pc
  | Stop

type t =
  { pc : pc
  ; op : opcode
  ; next : advance_type
  ; last_val : E.value option
  ; env : environment [@default Env.empty]
  ; stack : frame list
  }
[@@deriving make]

let init program pc = make ~pc ~op:(Vector.get program pc) ~next:(Jump pc) ()

let current_op state = state.op

let set_next next state = { state with next }

let current_next_pc state =
  match state.next with
  | Next -> Some (state.pc, state.pc + 1)
  | Jump pc -> Some (state.pc, pc)
  | Stop -> None

(* Updates the pc and op fields. Returns the new state wrapped in an option;
   returns None if execution is finished (because we reached Stop) *)
let advance program state =
  match current_next_pc state with
  | Some (_, pc) -> Some { state with pc; op = Vector.get program pc }
  | None -> None

let last_val state = state.last_val

let clear_last_val state = { state with last_val = None }

let lookup x state =
  match Env.find_opt x state.env with
  | Some v -> v
  | None -> raise (Object_not_found x)

(* Updating the environment will automatically update state.last_val *)
let update x v ?(last_val = None) state =
  let env = Env.add x v state.env in
  match last_val with
  | None -> { state with env; last_val = Some v }
  | Some v' -> { state with env; last_val = Some v' }

let push fn_pc fn_id params args state =
  let[@warning "-8"] (Call { target; _ }) = state.op in
  let stack = (state.pc, target, fn_id, state.env) :: state.stack in
  let env = List.fold_left2 (fun e x v -> Env.add x v e) Env.empty params args in
  { state with next = Jump fn_pc; last_val = None; env; stack }

let pop state =
  let (pc, target, _, env), stack = List.hd_tl state.stack in
  let env =
    match state.last_val with
    | None -> env
    | Some v -> Env.add target v env in
  { state with next = Jump (pc + 1); env; stack }

let show state =
  let op_str = show_pc_opcode state.pc state.op in
  let last_val_str =
    Option.map_or ~default:""
      (fun v -> Printf.sprintf "\t  res: %s\n" @@ E.show_val v)
      state.last_val in
  let env_str =
    let env_list =
      state.env |> Env.bindings |> List.filter (fun (x, _) -> not @@ String.contains x '$') in
    if List.is_empty env_list then ""
    else
      List.to_string ~start:"\t  env: " ~stop:"\n" ~sep:", "
        (fun (x, v) -> Printf.sprintf "%s ↦ %s" x (E.show_val v))
        env_list in
  let stack_str =
    let stack_list = List.map (fun (_, _, fn, _) -> fn) state.stack in
    if List.is_empty stack_list then ""
    else List.to_string ~start:"\t  stk: " ~stop:"\n" Fun.id stack_list in
  match[@warning "-4"] state.op with
  | Comment _ -> Printf.sprintf "%s\n" op_str
  | _ -> Printf.sprintf "%s\n%s%s%s" op_str last_val_str env_str stack_str

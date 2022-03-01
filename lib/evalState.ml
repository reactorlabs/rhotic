open Containers
open Common
open Util
open Opcode

module E = Expr

module Env = Map.Make (E.Identifier)
type environment = E.value Env.t
type frame = pc * E.identifier * environment
type advance_type =
  | Start
  | Next
  | Jump  of pc
  | Stop
type t =
  { program : opcode Vector.ro_vector
  ; pc : pc
  ; next_pc : advance_type [@default Start]
  ; last_val : E.value option
  ; env : environment [@default Env.empty]
  ; stack : frame list
  ; debug : bool [@default false]
  }
[@@deriving make]

let program_pc state = (state.program, state.pc)

(* Also resets next_pc and last_val *)
let set_program_pc program pc state = { state with program; pc; next_pc = Start; last_val = None }

let current_pc_op state = (state.pc, Vector.get state.program state.pc)

(* Sets the next_pc field but doesn't update the current pc. *)
let set_next_pc next_pc state = { state with next_pc }

(* Updates the pc field with the value of next_pc. Returns the new state wrapped in an option;
   returns None if execution is finished (because we reached Stop) *)
let advance state =
  match state.next_pc with
  | Start -> Some state
  | Next -> Some { state with pc = state.pc + 1 }
  | Jump pc -> Some { state with pc }
  | Stop -> None

let last_val state = state.last_val

let set_last_val last_val state = { state with last_val }

let lookup x state =
  match Env.find_opt x state.env with
  | Some v -> v
  | None -> raise (Object_not_found x)

(* Updating the environment will automatically update state.last_val *)
let update x v state = { state with env = Env.add x v state.env; last_val = Some v }

let push fn_pc fn_id params args state =
  let stack = (state.pc, fn_id, state.env) :: state.stack in
  let env = List.fold_left2 (fun e x v -> Env.add x v e) Env.empty params args in
  { state with next_pc = Jump fn_pc; env; stack }

let pop state =
  let (pc, _, env), stack = List.hd_tl state.stack in
  let[@warning "-8"] (Call { target; _ }) = Vector.get state.program pc in
  let env =
    match state.last_val with
    | None -> env
    | Some v -> Env.add target v env in
  { state with next_pc = Jump (pc + 1); env; stack }

let debug_print state =
  if state.debug then
    let op_str = show_pc_opcode state.pc (Vector.get state.program state.pc) in
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
      let stack_list = List.map (fun (_, fn, _) -> fn) state.stack in
      if List.is_empty stack_list then ""
      else List.to_string ~start:"\t  stk: " ~stop:"\n" Fun.id stack_list in
    Printf.eprintf "%s\n%s%s%s\n%!" op_str last_val_str env_str stack_str

let init ?(debug = false) () =
  let program = Vector.of_list [ Opcode.Start; Opcode.Stop ] in
  make ~program ~pc:0 ~debug ()

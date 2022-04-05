open Util

(* This analysis represents each program value by its type.
   This implementation uses dynamic information: it compute abstract states by abstracting the
   concrete state, instead of transforming the previous abstract state.
   In other words, the dynamic analysis will not use the transfer functions. *)

(* Include the type analysis implementation, i.e. lattice, astate definition.
   We'll override the transfer functions to abstract the concrete state.
   If no conrete state is provided, we'll fall back to TypeAnalysisImpl. *)
include TypeAnalysisImpl

let abstract_cstate (cstate : EvalState.t) =
  let pc, op = (cstate.pc, cstate.op) in
  let last_val = cstate.last_val |> Option.map_or ~default:AValue.Bot AValue.abstract_val in
  let env = cstate.env |> Env.map AValue.abstract_val in
  TypeAnalysisImpl.make_astate ~pc ~op ~last_val ~env ()

let call params args_se ?cstate astate =
  match cstate with
  | None -> TypeAnalysisImpl.call params args_se astate
  | Some cstate -> abstract_cstate cstate

let return targets ?cstate astate =
  match cstate with
  | None -> TypeAnalysisImpl.return targets astate
  | Some cstate -> abstract_cstate cstate

let step ?cstate astate =
  match cstate with
  | None -> TypeAnalysisImpl.step astate
  | Some cstate -> abstract_cstate cstate

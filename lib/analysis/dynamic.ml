open Util
open Opcode

(*
  TODO:
    - architecture
        - maybe need objects for late binding, so that eval can call observe for the right analysis
        - how to combine:
          - eval/analysis/dynamic modules

    - analysis that only looks at values
    - write another simple analysis (intervals?)
    - na analysis
 *)

module type S = sig
  type astate
  type ctx
  val make : ?debug:bool -> ?enable:bool -> Opcode.opcode Vector.ro_vector -> ctx
  val observe : EvalState.t -> ctx -> ctx
  val finish : ctx -> astate Vector.ro_vector
end

module Make (AI : Analysis.AnalysisInstance) : S = struct
  type astate = AI.astate
  type ctx =
    { program : opcode Vector.ro_vector
    ; analysis : astate array
    ; debug : bool
    ; enable : bool
    }

  let make ?(debug = false) ?(enable = false) program =
    { program; analysis = Vector.mapi AI.init program |> Vector.to_array; debug; enable }

  (* Apply a single step of the abstract evaluator (i.e. apply a transfer function).
      The transfer functions are split into call, return, and intraprocedural step functions. *)
  let step (pc, pc') ctx =
    let astate = ctx.analysis.(pc) in
    match[@warning "-4"] Vector.get ctx.program pc with
    | Call { params; args_se; _ } -> AI.call params args_se astate
    | Exit _ ->
        (* Instruction before the return site is function call *)
        let[@warning "-8"] (Call { target; _ }) = Vector.get ctx.program (pc' - 1) in
        AI.return [ target ] astate
    | _ -> AI.step astate

  let merge_at pc astate' ctx =
    let astate = ctx.analysis.(pc) in
    let merged = AI.merge astate astate' in
    ctx.analysis.(pc) <- merged ;
    if ctx.debug then (
      Printf.eprintf "\t  Before %d:\t%s\n%!" pc (AI.show astate) ;
      if not (AI.leq astate astate' && AI.leq astate' astate) then
        Printf.eprintf "\t  Merged %d:\t%s\n%!" pc (AI.show merged))

  let finish ctx =
    if ctx.debug then (
      Printf.eprintf "\nDynamic analysis output:\n" ;
      Array.iteri
        (fun pc astate ->
          Printf.eprintf "\t  Before %d:\t%s\n%!" pc (AI.show astate) ;
          Printf.eprintf "%s\n%!" @@ AI.show_op astate)
        ctx.analysis) ;
    Vector.freeze @@ Vector.of_array ctx.analysis

  let observe state ctx =
    if not ctx.enable then ctx
    else
      match EvalState.current_next_pc state with
      | None -> ctx
      | Some (pc, pc') ->
          let astate' = step (pc, pc') ctx in
          if ctx.debug then (
            Printf.eprintf "%s\n%!" @@ AI.show_op astate' ;
            Printf.eprintf "\t  After  %d:\t%s\n%!" pc (AI.show astate')) ;
          merge_at pc' astate' ctx ;
          ctx
end

module TypeAnalysis = Make (TypeAnalysisImpl)

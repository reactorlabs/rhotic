open Util

(*
  TODO:
    - check that it "works"
    - write some examples

    - architecture
        - maybe need objects for late binding, so that eval can call observe for the right analysis
        - how to combine:
          - static/dynamic context
          - evalState/aState
          - eval/analysis/dynamic modules
        - how to clean up astate

    - write another simple analysis (intervals?)
    - analysis that only looks at values
    - na analysis
 *)

module type S = sig
  type astate
  type ctx
  val make : ?debug:bool -> ?enable:bool -> Opcode.opcode Vector.ro_vector -> ctx
  val observe : EvalState.t -> ctx -> ctx
end

module Make (AI : Analysis.AnalysisInstance) : S = struct
  type astate = AI.astate
  type ctx =
    { analysis : astate array
    ; debug : bool
    ; enable : bool
    }

  let make ?(debug = false) ?(enable = false) program =
    let n = Vector.length program in
    (* TODO: For the dynamic analysis, we only care about AI.state's last_val and env, so use dummy values *)
    let analysis =
      Array.init n (fun pc -> AI.init pc (Vector.get program pc) "" List.empty List.empty) in
    { analysis; debug; enable }

  let get pc ctx = ctx.analysis.(pc)

  let merge_at pc astate' ctx =
    let astate = ctx.analysis.(pc) in
    let merged = AI.merge astate astate' in
    ctx.analysis.(pc) <- merged

  let observe state ctx =
    if not ctx.enable then ctx
    else
      let _, pc = EvalState.program_pc state in
      match EvalState.next_pc state with
      | None -> ctx
      | Some pc' ->
          let astate = get pc ctx in
          let astate' = AI.step astate in
          if ctx.debug then Printf.eprintf "\t  <dynamic %d>:\t%s\n%!" pc' (AI.show astate') ;
          merge_at pc' astate' ctx ;
          ctx
end

module TypeAnalysis = Make (TypeAnalysisImpl)

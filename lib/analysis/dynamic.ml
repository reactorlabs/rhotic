open Util
open Opcode
open Analysis

module Make (AI : AnalysisInstance) : S = struct
  type astate = AI.astate

  module Context = struct
    type t =
      { program : opcode Vector.ro_vector
      ; analysis : astate array
      ; debug : bool
      }

    let make ?(debug = false) program =
      { program; analysis = Vector.mapi AI.init program |> Vector.to_array; debug }

    (* Apply a single step of the abstract evaluator (i.e. apply a transfer function).
        The transfer functions are split into call, return, and intraprocedural step functions. *)
    let step (pc, pc') cstate ctx =
      let astate = ctx.analysis.(pc) in
      match[@warning "-4"] Vector.get ctx.program pc with
      | Call { params; args_se; _ } -> AI.call params args_se ~cstate astate
      | Exit _ ->
          (* Instruction before the return site is function call *)
          let[@warning "-8"] (Call { target; _ }) = Vector.get ctx.program (pc' - 1) in
          AI.return [ target ] ~cstate astate
      | _ -> AI.step ~cstate astate

    let merge_at pc astate' ctx =
      let astate = ctx.analysis.(pc) in
      let merged = AI.merge astate astate' in
      ctx.analysis.(pc) <- merged ;
      if ctx.debug then (
        Printf.printf "\t  Before %d:\t%s\n%!" pc (AI.show astate) ;
        if not (AI.leq astate astate' && AI.leq astate' astate) then
          Printf.printf "\t  Merged %d:\t%s\n%!" pc (AI.show merged))

    let finish ctx =
      if ctx.debug then (
        Printf.printf "\nDynamic analysis output:\n" ;
        Array.iteri
          (fun pc astate ->
            Printf.printf "\t  Before %d:\t%s\n%!" pc (AI.show astate) ;
            Printf.printf "%s\n%!" @@ AI.show_op astate)
          ctx.analysis) ;
      Vector.freeze @@ Vector.of_array ctx.analysis

    let observe cstate ctx =
      match EvalState.current_next_pc cstate with
      | None -> ctx
      | Some (pc, pc') ->
          let astate' = step (pc, pc') cstate ctx in
          if ctx.debug then (
            Printf.printf "%s\n%!" @@ AI.show_op astate' ;
            Printf.printf "\t  After  %d:\t%s\n%!" pc (AI.show astate')) ;
          merge_at pc' astate' ctx ;
          ctx
  end

  let analyze ?(debug = false) (program, pc) =
    let rec eval_continuous state ctx =
      match EvalState.advance program state with
      | None -> (state, ctx)
      | Some state ->
          let state' = Eval.step state in
          let ctx' = Context.observe state' ctx in
          (eval_continuous [@tailcall]) state' ctx' in

    let state = EvalState.init program pc in
    let ctx = Context.make ~debug program in

    eval_continuous state ctx |> Stdlib.snd |> Context.finish
end

module TypeAnalysis = Make (TypeAnalysisImpl)
module IntervalAnalysis = Make (IntervalAnalysisImpl)

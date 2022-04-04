open Containers
open Util
open Opcode
open Analysis

module Make (AI : AnalysisInstance) : S = struct
  type astate = AI.astate

  module Context = struct
    type t =
      { program : opcode Vector.ro_vector
      ; cfg : pc list Vector.ro_vector (* mapping of pc to successor pcs *)
      ; analysis : astate array (* all the analysis states we compute *)
      ; worklist : pc Stack.t
      ; seen : bool array (* make sure we visit each node at least once *)
      ; debug : bool
      }

    let make ?(debug = false) program entry_pc =
      let n = Vector.length program in

      (* For the given pc and op, return the successor pcs *)
      let successors pc op =
        match op with
        | Stop -> []
        | Copy _ | Builtin _ | Print _ | Nop | Start | Entry _ | Comment _ -> [ pc + 1 ]
        | Jump pc' | Call { fn_pc = pc'; _ } -> [ pc' ]
        | Branch (_, pc') -> [ pc + 1; pc' ]
        | Exit f ->
            (* Need to find all sites that call f; successor is the nop after the callsite *)
            let return_sites acc pc op =
              match[@warning "-4"] op with
              | Call { fn; _ } when String.equal fn f -> (pc + 1) :: acc
              | _ -> acc in
            Vector.foldi return_sites List.empty program in

      let ctx =
        { program
        ; cfg = Vector.mapi successors program
        ; analysis = Vector.mapi AI.init program |> Vector.to_array
        ; worklist = Stack.create ()
        ; seen = Array.make n false
        ; debug
        } in
      Stack.push entry_pc ctx.worklist ;
      ctx

    let is_done ctx =
      (if ctx.debug then
       let str = Stack.to_list ctx.worklist |> List.to_string Int.to_string in
       Printf.printf "\t  Worklist: %s\n%!" str) ;
      Stack.is_empty ctx.worklist

    let pop ctx =
      let pc = Stack.pop ctx.worklist in
      let state = ctx.analysis.(pc) in
      if ctx.debug then Printf.printf "%s\n%!" @@ AI.show_op state ;
      ctx.seen.(pc) <- true ;
      (pc, state)

    let get pc ctx = ctx.analysis.(pc)

    let successors pc ctx = Vector.get ctx.cfg pc

    (* Apply a single step of the abstract evaluator (i.e. apply a transfer function).
       The transfer functions are split into call, return, and intraprocedural step functions. *)
    let step pc state ctx =
      match[@warning "-4"] Vector.get ctx.program pc with
      | Call { params; args_se; _ } -> AI.call params args_se state
      | Exit _ ->
          let targets =
            Vector.get ctx.cfg pc
            |> List.map (fun pc ->
                   (* Instruction before the return site is function call *)
                   let[@warning "-8"] (Call { target; _ }) = Vector.get ctx.program (pc - 1) in
                   target) in
          AI.return targets state
      | _ -> AI.step state

    let merge_at pc state' ctx =
      let state = ctx.analysis.(pc) in
      let merged = AI.merge state state' in
      ctx.analysis.(pc) <- merged ;
      Stack.push pc ctx.worklist ;
      if ctx.debug then Printf.printf "\t  Merged %d:\t%s\n%!" pc (AI.show merged)

    let add_if_not_seen pc ctx = if not ctx.seen.(pc) then Stack.push pc ctx.worklist

    let finish ctx =
      if ctx.debug then (
        Printf.printf "\nStatic analysis output:\n" ;
        Array.iteri
          (fun pc state ->
            Printf.printf "\t  Before %d:\t%s\n%!" pc (AI.show state) ;
            Printf.printf "%s\n%!" @@ AI.show_op state)
          ctx.analysis) ;
      Vector.freeze @@ Vector.of_array ctx.analysis
  end

  let analyze ?(debug = false) (program, entry_pc) =
    let ctx = Context.make ~debug program entry_pc in

    (* Main analysis loop *)
    while not @@ Context.is_done ctx do
      (* Pop state from worklist, then apply transfer function (i.e. abstract step) *)
      let pc, state = Context.pop ctx in
      let state' = Context.step pc state ctx in
      if debug then Printf.printf "\t  After  %d:\t%s\n%!" pc (AI.show state') ;

      (* Compare the new state to the successors' states *)
      Context.successors pc ctx
      |> List.iter (fun pc' ->
             let succ_state = Context.get pc' ctx in
             if debug then Printf.printf "\t  Before %d:\t%s\n%!" pc' (AI.show succ_state) ;

             (* If the new state is strictly greater than the successor state,
                merge/update the sucessor state.
                Otherwise, make sure we visit the successor if we haven't already. *)
             if not @@ AI.leq state' succ_state then Context.merge_at pc' state' ctx
             else Context.add_if_not_seen pc' ctx)
    done ;

    (* Return analysis vector *)
    Context.finish ctx
end

module TypeAnalysis = Make (TypeAnalysisImpl)
module IntervalAnalysis = Make (IntervalAnalysisImpl)

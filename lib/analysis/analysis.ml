open Containers
open Util
open Opcode

module A = TypeAnalysis

module Context = struct
  exception Found_function of string

  (* TODO: forwards/backwards, entry/exit pc
           much harder: context sensitivity *)
  type t =
    { analysis : A.state array (* all the analysis states we compute *)
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
      | Jump pc' -> [ pc' ]
      | Branch (_, pc') -> [ pc + 1; pc' ]
      | Call { fn_pc; _ } -> [ fn_pc ]
      | Exit f ->
          (* Need to find all sites that call f; successor is the nop after the callsite *)
          let return_sites acc pc op =
            match[@warning "-4"] op with
            | Call { fn; _ } when String.equal fn f -> (pc + 1) :: acc
            | _ -> acc in
          Vector.foldi return_sites List.empty program in

    (* For the given pc, return the name of function it's in *)
    let current_function pc =
      try
        (* From the current pc, advance until we find an "Exit" or "Stop" instruction *)
        for i = pc to n - 1 do
          match[@warning "-4"] Vector.get program i with
          | Exit id -> raise @@ Found_function id
          | Stop -> raise @@ Found_function ""
          | _ -> ()
        done ;
        assert false
      with Found_function id -> id in

    (* If the given op is an "Exit", find the target registers at the callsites of the function;
       these are the variables that will store the return value *)
    let return_id pc op =
      match[@warning "-4"] op with
      | Exit _ ->
          successors pc op
          |> List.map (fun pc ->
                 let[@warning "-8"] (Call { target; _ }) = Vector.get program (pc - 1) in
                 current_function pc ^ "$$" ^ target)
      | _ -> [] in

    let cfg = program |> Vector.mapi successors in
    let fns = program |> Vector.mapi (fun pc _ -> current_function pc) in
    let ids = program |> Vector.mapi return_id in

    let ctx =
      { analysis =
          Array.init n (fun pc ->
              A.init pc (Vector.get program pc) (Vector.get cfg pc) (Vector.get fns pc)
                (Vector.get ids pc))
      ; worklist = Stack.create ()
      ; seen = Array.make n false
      ; debug
      } in
    Stack.push entry_pc ctx.worklist ;
    ctx

  let is_done ctx =
    (if ctx.debug then
     let str = Stack.to_list ctx.worklist |> List.to_string Int.to_string in
     Printf.eprintf "\t  Worklist: %s\n%!" str) ;
    Stack.is_empty ctx.worklist

  let pop ctx =
    let pc = Stack.pop ctx.worklist in
    let state = ctx.analysis.(pc) in
    if ctx.debug then Printf.eprintf "%s\n%!" @@ A.show_op state ;
    ctx.seen.(pc) <- true ;
    (pc, state)

  let get pc ctx = ctx.analysis.(pc)

  let merge_at pc state' ctx =
    let state = ctx.analysis.(pc) in
    let merged = A.merge state state' in
    ctx.analysis.(pc) <- merged ;
    if ctx.debug then Printf.eprintf "\t  Merged %d:\t%s\n%!" pc (A.show merged) ;
    Stack.push pc ctx.worklist

  let add_if_not_seen pc ctx = if not ctx.seen.(pc) then Stack.push pc ctx.worklist

  let finish ctx =
    let analysis, analysis' = (ctx.analysis, Array.map A.step ctx.analysis) in
    if ctx.debug then (
      Printf.eprintf "\nAnalysis output:\n" ;
      Array.iteri
        (fun pc _ ->
          let state, state' = (analysis.(pc), analysis'.(pc)) in
          Printf.eprintf "\t  Before %d:\t%s\n%!" pc (A.show state) ;
          Printf.eprintf "%s\n%!" @@ A.show_op state ;
          Printf.eprintf "\t  After  %d:\t%s\n%!" pc (A.show state'))
        analysis) ;
    (Vector.freeze @@ Vector.of_array analysis, Vector.freeze @@ Vector.of_array analysis')
end

let analyze ?(debug = false) program entry_pc =
  let ctx = Context.make ~debug program entry_pc in

  while not @@ Context.is_done ctx do
    (* Pop state from worklist, then apply transfer function (i.e. abstract step) *)
    let pc, state = Context.pop ctx in
    let state' = A.step state in
    if debug then Printf.eprintf "\t  After  %d:\t%s\n%!" pc (A.show state') ;

    (* Compare the new state to the successors' states *)
    A.successors state'
    |> List.iter (fun pc' ->
           let succ_state = Context.get pc' ctx in
           if debug then Printf.eprintf "\t  Before %d:\t%s\n%!" pc' (A.show succ_state) ;

           (* If the new state is strictly greater than the successor state,
              merge/update the sucessor state.
              Otherwise, make sure we visit the successor if we haven't already. *)
           if not @@ A.leq state' succ_state then Context.merge_at pc' state' ctx
           else Context.add_if_not_seen pc' ctx)
  done ;

  (* Compute the after states; return a pair of vectors for the before/after states *)
  Context.finish ctx

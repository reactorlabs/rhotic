open Containers
open Expr
open Util

module O = Opcode

module FunTab = Map.Make (Identifier)
type function_table = (O.pc * identifier list) FunTab.t

(* Fixup function calls.
    First pass: find all Entry opcodes to populate function table
    Second pass: find and update Call opcodes *)
let fixup_callsites program () =
  let update_table fun_tab i opcode =
    match[@warning "-4"] opcode with
    | O.Entry (id, params) -> FunTab.add id (i, params) fun_tab
    | _ -> fun_tab in
  let fixup fun_tab opcode =
    match[@warning "-4"] opcode with
    | O.Call { target; fn; args_se; _ } -> (
        match FunTab.find_opt fn fun_tab with
        | None -> raise (Common.Function_not_found fn)
        | Some (fn_pc, params) ->
            let n1, n2 = (List.length args_se, List.length params) in
            if n1 <> n2 then raise (Common.Invalid_number_of_args { expected = n2; received = n1 }) ;
            O.Call { target; fn; fn_pc; params; args_se })
    | _ -> opcode in

  let funtab = Vector.foldi update_table FunTab.empty program in
  Vector.map_in_place (fixup funtab) program

let compile ?(program = O.empty_program) stmts =
  let fresh_compile = Vector.equal O.equal_opcode program O.empty_program in
  let buffer =
    if fresh_compile then Vector.create_with ~capacity:(List.length stmts) O.Nop
    else Vector.copy program in

  let current_pc () = Vector.size buffer in
  let push_op = Vector.push buffer in
  let set_op = Vector.set buffer in

  let counter = ref 0 in
  let gensym ?(pre = "tmp") () =
    let pc = current_pc () in
    counter := !counter + 1 ;
    Printf.sprintf "%s$%d_%d" pre pc !counter in

  let rec compile_stmts stmt = List.iter compile_stmt stmt
  and compile_stmt stmt =
    let compile_expr target = function
      | Combine ses -> push_op @@ O.builtin target Combine ses
      | Dataframe_Ctor _ -> raise Not_supported
      | Unary_Op (op, se) -> push_op @@ O.builtin target (Unary op) [ se ]
      | Binary_Op (op, se1, se2) -> push_op @@ O.builtin target (Binary op) [ se1; se2 ]
      | Subset1 (se1, None) -> push_op @@ O.builtin target Subset1 [ se1 ]
      | Subset1 (se1, Some se2) -> push_op @@ O.builtin target Subset1 [ se1; se2 ]
      | Subset2 (se1, se2) -> push_op @@ O.builtin target Subset2 [ se1; se2 ]
      | Call (".input", args) -> push_op @@ O.builtin target Input args
      | Call (fn, args) ->
          (* fn_pc and params are placeholders for now *)
          let params = List.map (fun _ -> "?") args in
          push_op @@ O.Call { target; fn; fn_pc = -1; params; args_se = args } ;
          (* Ensure there's always an instruction after Call *)
          push_op O.Nop
      | Simple_Expression se -> push_op @@ O.copy target se in

    let compile_if cond true_branch false_branch =
      push_op @@ Comment (Printf.sprintf "if %s" (show_simple_expression cond)) ;

      (* Branch cond true_pc *)
      let branch_pc = current_pc () in
      push_op Nop ;

      (* body of false_branch *)
      compile_stmts false_branch ;

      (* Jump endif_pc *)
      let jump_pc = current_pc () in
      push_op Nop ;

      (* true_pc: body of true_branch *)
      let true_pc = current_pc () in
      compile_stmts true_branch ;

      (* endif_pc: Nop *)
      let endif_pc = current_pc () in
      push_op Nop ;

      (* Fixup placeholders *)
      set_op branch_pc @@ Branch (cond, true_pc) ;
      set_op jump_pc @@ Jump endif_pc ;

      push_op @@ Comment "end if" in

    let compile_for x seq body =
      push_op @@ Comment (Printf.sprintf "for %s in %s" x (show_simple_expression seq)) ;

      (* len$ = length(seq) *)
      let len = gensym ~pre:"len" () in
      push_op @@ O.builtin len (Unary Length) [ seq ] ;

      (* i = 0 *)
      let i = gensym ~pre:"i" () in
      compile_expr i (Simple_Expression (Lit (Int 0))) ;

      (* begin_pc: cond = i >= len *)
      let begin_pc = current_pc () in
      let cond = gensym ~pre:"cond" () in
      push_op @@ O.builtin cond (Binary (Relational Greater_Equal)) [ Var i; Var len ] ;

      (* Branch cond end_pc *)
      let branch_pc = current_pc () in
      push_op Nop ;

      (* i = i + 1 *)
      push_op @@ O.builtin i (Binary (Arithmetic Plus)) [ Var i; Lit (Int 1) ] ;

      (* x = seq[i] *)
      push_op @@ O.builtin x Subset2 [ seq; Var i ] ;

      (* body *)
      push_op @@ Comment "for body" ;
      compile_stmts body ;

      (* jump begin_pc *)
      push_op @@ Jump begin_pc ;

      (* end_pc: nop *)
      let end_pc = current_pc () in
      push_op Nop ;

      (* Fixup placeholders *)
      set_op branch_pc @@ Branch (Var cond, end_pc) ;

      push_op @@ Comment "end for" in

    match stmt with
    | Assign (x, e) -> compile_expr x e
    | Subset1_Assign (x1, ose2, se3) ->
        let tmp = gensym () in
        let args =
          match ose2 with
          | None -> [ Var tmp; se3 ]
          | Some se2 -> [ Var tmp; se2; se3 ] in
        push_op @@ O.copy tmp (Var x1) ;
        push_op @@ O.builtin x1 Subset1_Assign args
    | Subset2_Assign (x1, se2, se3) ->
        let tmp = gensym () in
        push_op @@ O.copy tmp (Var x1) ;
        push_op @@ O.builtin x1 Subset2_Assign [ Var tmp; se2; se3 ]
    | Function_Def _ ->
        (* Functions are compiled separately; see below. *)
        ()
    | If (cond, true_branch, false_branch) -> compile_if cond true_branch false_branch
    | For (x, seq, body) -> compile_for x seq body
    | Print e -> (
        match[@warning "-4"] e with
        | Simple_Expression se -> push_op @@ Print se
        | _ ->
            let tmp = gensym () in
            compile_expr tmp e ;
            push_op @@ Print (Var tmp))
    | Expression e ->
        let tmp = gensym () in
        compile_expr tmp e in

  (* If this is not a fresh compile, remove the old Stop instruction. *)
  (if not fresh_compile then
   let removed = Vector.pop_exn buffer in
   assert (O.equal_opcode removed Stop)) ;

  (* Compile functions first.
     Note: The opcode semantics will be different from how R handles functions,
     because we compile functions in two passes and lift all function definitions to the top.
     R is more dynamic, and populates the environment whenever a function definition is evaluated. *)
  List.iter
    (function[@warning "-4"]
      | Function_Def (id, params, body) ->
          (* jump after_exit
              Normally unreachable, unless we compile code on demand (e.g. in a repl or with eval).
              So if we get here, jump past the function definition. *)
          let before_entry = current_pc () in
          push_op Nop ;

          (* Function entry, body, and exit *)
          push_op @@ Comment (Printf.sprintf "function %s" id) ;
          push_op @@ Entry (id, params) ;
          compile_stmts body ;
          push_op @@ Exit id ;

          (* after_exit: comment *)
          let after_exit = current_pc () in
          push_op @@ Comment "end function" ;
          set_op before_entry @@ Jump after_exit
      | _ -> ())
    stmts ;

  (* Compile main.
     If this is a fresh compile (no pre-existing program) add the comment and Start instruction *)
  let start_pc =
    if fresh_compile then (
      push_op @@ Comment "start main" ;
      push_op Start ;
      (* Refer to the pc of Start, not the next instruction. *)
      current_pc () - 1)
    else current_pc () in
  compile_stmts stmts ;
  push_op Stop ;

  fixup_callsites buffer () ;

  (Vector.freeze buffer, start_pc)

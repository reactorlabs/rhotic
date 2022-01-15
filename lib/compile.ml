open Containers
open Expr
open Util

module FunTab = Map.Make (Identifier)
type function_table = (Opcode.pc * identifier list) FunTab.t

let compile_program stmts =
  let buffer = Vector.create_with ~capacity:(List.length stmts) Opcode.Nop in
  let counter = ref 0 in

  let current_pc () = Vector.size buffer in
  let push_op = Vector.push buffer in
  let set_op = Vector.set buffer in

  let gensym ?(pre = "tmp") () =
    counter := !counter + 1 ;
    Printf.sprintf "%s$%d" pre !counter in

  let rec compile_stmts stmt = List.iter compile_stmt stmt
  and compile_stmt stmt =
    let compile_expr target = function
      | Combine ses -> Opcode.builtin target Combine ses
      | Dataframe_Ctor _ -> raise Not_supported
      | Unary_Op (op, se) -> Opcode.builtin target (Unary op) [ se ]
      | Binary_Op (op, se1, se2) -> Opcode.builtin target (Binary op) [ se1; se2 ]
      | Subset1 (se1, None) -> Opcode.builtin target Subset1 [ se1 ]
      | Subset1 (se1, Some se2) -> Opcode.builtin target Subset1 [ se1; se2 ]
      | Subset2 (se1, se2) -> Opcode.builtin target Subset2 [ se1; se2 ]
      | Call (".input", args) -> Opcode.builtin target Input args
      | Call (fn, args) ->
          (* fn_pc and params are placeholders for now *)
          let params = List.map (fun _ -> "?") args in
          Opcode.Call { target; fn; fn_pc = -1; params; args }
      | Simple_Expression se -> Opcode.copy target se in

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
      set_op branch_pc (Branch (cond, true_pc)) ;
      set_op jump_pc (Jump endif_pc) ;

      push_op @@ Comment "end if" in

    let compile_for x seq body =
      push_op @@ Comment (Printf.sprintf "for %s in %s" x (show_simple_expression seq)) ;

      (* len$ = length(seq) *)
      let len = gensym ~pre:"len" () in
      push_op @@ Opcode.builtin len Length [ seq ] ;

      (* i = 0 *)
      let i = gensym ~pre:"i" () in
      push_op @@ compile_expr i (Simple_Expression (Lit (Int 0))) ;

      (* begin_pc: cond = i >= len *)
      let begin_pc = current_pc () in
      let cond = gensym ~pre:"cond" () in
      push_op @@ Opcode.builtin cond (Binary (Relational Greater_Equal)) [ Var i; Var len ] ;

      (* Branch cond end_pc *)
      let branch_pc = current_pc () in
      push_op Nop ;

      (* i = i + 1 *)
      push_op @@ Opcode.builtin i (Binary (Arithmetic Plus)) [ Var i; Lit (Int 1) ] ;

      (* x = seq[i] *)
      push_op @@ Opcode.builtin x Subset2 [ seq; Var i ] ;

      (* body *)
      push_op @@ Comment "for body" ;
      compile_stmts body ;

      (* jump begin_pc *)
      push_op @@ Jump begin_pc ;

      (* end_pc: nop *)
      let end_pc = current_pc () in
      push_op Nop ;

      (* Fixup placeholders *)
      set_op branch_pc (Branch (Var cond, end_pc)) ;

      push_op @@ Comment "end for" in

    match stmt with
    | Assign (x, e) -> push_op @@ compile_expr x e
    | Subset1_Assign (x1, ose2, se3) ->
        let tmp = gensym () in
        let args =
          match ose2 with
          | None -> [ Var tmp; se3 ]
          | Some se2 -> [ Var tmp; se2; se3 ] in
        push_op @@ Opcode.copy tmp (Var x1) ;
        push_op @@ Opcode.builtin x1 Subset1_Assign args
    | Subset2_Assign (x1, se2, se3) ->
        let tmp = gensym () in
        push_op @@ Opcode.copy tmp (Var x1) ;
        push_op @@ Opcode.builtin x1 Subset2_Assign [ Var tmp; se2; se3 ]
    | Function_Def (_, _, _) -> ()
    | If (cond, true_branch, false_branch) -> compile_if cond true_branch false_branch
    | For (x, seq, body) -> compile_for x seq body
    | Print e -> (
        match[@warning "-4"] e with
        | Simple_Expression se -> push_op @@ Print se
        | _ ->
            let tmp = gensym () in
            push_op @@ compile_expr tmp e ;
            push_op @@ Print (Var tmp))
    | Expression e ->
        let tmp = gensym () in
        push_op @@ compile_expr tmp e in

  (* Compile functions first.
     Note: The opcode semantics will be different from the AST interpreter,
     because we compile functions in two passes and lift all function
     definitions to the top. The AST interpreter can only call a function if
     that function's definition was evaluated. *)
  List.iter
    (function[@warning "-4"]
      | Function_Def (id, params, body) ->
          push_op @@ Comment (Printf.sprintf "function %s" id) ;
          push_op @@ Entry (id, params) ;
          compile_stmts body ;
          push_op (Exit id) ;
          push_op (Comment "end function")
      | _ -> ())
    stmts ;

  (* Compile main *)
  push_op (Comment "start main") ;
  push_op Start ;
  compile_stmts stmts ;
  push_op Stop ;

  (* Fixup function calls.
     First pass: find all Entry opcodes to populate function table
     Second pass: find and update Call opcodes *)
  let fun_tab =
    Vector.foldi
      (fun fun_tab i opcode ->
        match[@warning "-4"] opcode with
        | Opcode.Entry (id, params) -> FunTab.add id (i, params) fun_tab
        | _ -> fun_tab)
      FunTab.empty buffer in

  Vector.map_in_place
    (fun opcode ->
      match[@warning "-4"] opcode with
      | Opcode.Call { target; fn; args; _ } -> (
          match FunTab.find_opt fn fun_tab with
          | None -> raise (Common.Function_not_found fn)
          | Some (fn_pc, params) -> Call { target; fn; fn_pc; params; args })
      | _ -> opcode)
    buffer ;

  Vector.freeze buffer

(* TODO:
   - concrete interpreter

   - abstract interpreter
     - need some abstract state / analysis
   - dynamic interpreter
*)

open Containers
open Util
open Expr

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

(* Fixup var identifiers so that each identifier is unique; this makes analysis a bit easier.
   To make identifiers more readable, we prefix with the function name. *)
let fixup_vars program () =
  let prefix_op pre op =
    let prefix_id ?(pre = pre) = Identifier.prefix ~pre in
    let prefix_ids ?(pre = pre) = List.map (prefix_id ~pre) in
    let prefix_se ?(pre = pre) = function
      | Lit l -> Lit l
      | Var x -> Var (prefix_id ~pre x) in
    let prefix_ses ?(pre = pre) = List.map (prefix_se ~pre) in

    match op with
    | O.Copy (id, se) -> O.Copy (prefix_id id, prefix_se se)
    | O.Call { target; fn; fn_pc; params; args_se } ->
        Call
          { target = prefix_id target
          ; fn
          ; fn_pc
          ; params = prefix_ids ~pre:fn params
          ; args_se = prefix_ses args_se
          }
    | O.Builtin (id, builtin, ses) -> O.Builtin (prefix_id id, builtin, prefix_ses ses)
    | O.Entry (fn, params) -> O.Entry (fn, prefix_ids ~pre:fn params)
    | O.Branch (se, pc) -> O.Branch (prefix_se se, pc)
    | O.Print se -> O.Print (prefix_se se)
    | O.Nop | O.Exit _ | O.Start | O.Stop | O.Jump _ | O.Comment _ -> op in

  let cur_fun = ref "" in
  for pc = 0 to Vector.length program - 1 do
    let op = Vector.get program pc in
    (match[@warning "-4"] op with
    | O.Start -> cur_fun := ""
    | O.Entry (fn, _) -> cur_fun := fn
    | _ -> ()) ;
    Vector.set program pc (prefix_op !cur_fun op)
  done

let compile stmts =
  let buffer = Vector.create_with ~capacity:(List.length stmts) O.Nop in

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
      | Dataframe_Ctor _ -> raise Common.Not_supported
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

      (* len = length(seq) *)
      let len = gensym ~pre:"len" () in
      push_op @@ O.builtin len (Unary Length) [ seq ] ;

      (* i = 0 *)
      let i = gensym ~pre:"idx" () in
      compile_expr i (Simple_Expression (Lit (Int 0))) ;

      (* begin_pc: for header *)
      let begin_pc = current_pc () in
      push_op @@ Comment "for header" ;

      (* cond = i >= len *)
      let cond = gensym ~pre:"cnd" () in
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

  (* Compile functions first.
     Note: The opcode semantics will be different from how R handles functions,
     because we compile functions in two passes and lift all function definitions to the top.
     R is more dynamic, and populates the environment whenever a function definition is evaluated. *)
  List.iter
    (function[@warning "-4"]
      | Function_Def (id, params, body) ->
          (* Function entry, body, and exit *)
          push_op @@ Comment (Printf.sprintf "function %s" id) ;
          push_op @@ Entry (id, params) ;
          compile_stmts body ;
          push_op @@ Exit id ;
          push_op @@ Comment "end function"
      | _ -> ())
    stmts ;

  (* Compile main. Add the comment and Start instruction *)
  push_op @@ Comment "start main" ;
  let start_pc = current_pc () in
  push_op Start ;
  compile_stmts stmts ;
  push_op Stop ;

  fixup_callsites buffer () ;
  fixup_vars buffer () ;

  (Vector.freeze buffer, start_pc)

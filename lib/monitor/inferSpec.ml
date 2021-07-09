open Expr
open Common
open Util

(* TODO:
  - function-level constraints?
  - separate models for separate function calls, e.g. call-site sensitivity

  - other constraints (e.g. types)
  - better abstraction/modularity for constraints
    - how to run with multiple constraints
*)

module ConstraintNotNA = struct
  type constr =
    | May_be_NA
    | Must_not_be_NA

  type constraint_map = constr Env.t
  type dependency_map = VarSet.t Env.t

  type local_state =
    { constrs : constraint_map
    ; deps : dependency_map
    }
  type t = local_state FunTab.t

  let empty = { constrs = Env.empty; deps = Env.empty }

  let rec propagate (deps : dependency_map) (constrs : constraint_map) =
    (* Initial accumulator is the constraints map.
       We iterate over the constraints and for each v, add v's dependencies to the accumulator. *)
    let constrs' =
      Env.fold
        (fun x _ acc ->
          let new_deps = Env.get_or x deps ~default:VarSet.empty in
          VarSet.fold (fun x acc -> Env.add x Must_not_be_NA acc) new_deps acc)
        constrs constrs in
    if constrs' = constrs then constrs else propagate deps constrs'

  let add_constraints (cur_fun : identifier) (xs : VarSet.t) (state : t) =
    let { constrs; deps } = FunTab.get_or cur_fun state ~default:empty in
    let constrs =
      VarSet.fold (fun v acc -> Env.add v Must_not_be_NA acc) xs constrs |> propagate deps in
    Env.add cur_fun { constrs; deps } state

  let add_deps (cur_fun : identifier) (x : identifier) (vars : VarSet.t) (state : t) =
    let { constrs; deps } = FunTab.get_or cur_fun state ~default:empty in
    let old_vars = Env.get_or x deps ~default:VarSet.empty in
    let deps = Env.add x (VarSet.union old_vars vars) deps in
    let constrs = propagate deps constrs in
    Env.add cur_fun { constrs; deps } state
end

type stack_frame =
  { fun_id : identifier [@default Common.main_function]
  ; deps : VarSet.t Env.t [@default Env.empty]
  ; cur_vars : VarSet.t [@default VarSet.empty]
  }
[@@deriving make]

class monitor =
  object (self)
    inherit Monitor.monitor

    val mutable state : ConstraintNotNA.t = FunTab.empty

    val mutable stack : stack_frame list = [ make_stack_frame () ]

    val mutable last_popped_frame : stack_frame option = None

    (* Push a frame onto the shadow stack, and clear last_popped_frame.
       We always expect a non-empty stack because main$ should be on it. *)
    method private push_stack frame =
      assert (List.length stack > 0) ;

      let n = List.length stack - 1 in
      Stdlib.print_string (String.make (n * 2) ' ') ;
      Printf.printf "--> Entering %s\n" frame.fun_id ;

      stack <- frame :: stack ;
      last_popped_frame <- None

    (* Pop a frame from the shadow stack, set last_popped_frame, and return the frame.
       We always expect a non-empty stack because main$ should be on it. *)
    method private pop_stack =
      assert (List.length stack > 0) ;
      let top, rest = (List.hd stack, List.tl stack) in
      stack <- rest ;
      last_popped_frame <- Some top ;

      let n = List.length stack - 1 in
      Stdlib.print_string (String.make (n * 2) ' ') ;
      Printf.printf "<-- Exiting %s\n" top.fun_id ;

      top

    (* In a sequence expression x:y, x and y must not be NA. *)
    method! record_binary_op
        (conf : configuration)
        (op : binary_op)
        ((se1, _) : simple_expression * value)
        ((se2, _) : simple_expression * value)
        (_ : value) : unit =
      match op with
      | Seq ->
          let vars = VarSet.union (VarSet.collect_se se1) (VarSet.collect_se se2) in
          state <- ConstraintNotNA.add_constraints conf.cur_fun vars state
      | Arithmetic _ | Relational _ | Logical _ -> ()

    (* In a subset2 expression x[[i]], i must not be NA. *)
    method! record_subset2
        (conf : configuration)
        (_ : simple_expression * value)
        ((se, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state

    (* Entering a function call, so we need to initialize a frame and push onto the shadow stack.
       We initialize deps so that each param maps to a singleton set containing itself,
       e.g. x -> {x}, y -> {y} *)
    method! record_call_entry
        (conf : configuration) (fun_id : identifier) (_ : simple_expression list * value list)
        : unit =
      (* TODO: Need to handle call args later? *)
      let map_self map x = Env.add x (VarSet.singleton x) map in
      let params = Stdlib.fst @@ FunTab.find fun_id conf.fun_tab in
      let deps = List.fold_left map_self Env.empty params in
      self#push_stack @@ make_stack_frame ~fun_id ~deps ()

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      (* TODO: Need to handle returning the result of a function call? *)
      let top = self#pop_stack in
      assert (top.fun_id = fun_id)

    (* TODO: Clean up and document this. Then see which other assignment statements need to update deps. *)
    method! record_assign (conf : configuration) (x : identifier) ((e, _) : expression * value)
        : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_e e) state ;

          (* Collect vars, update deps, update last-seen-vars *)
          let ({ fun_id; deps; _ } as frame), rest = (List.hd stack, List.tl stack) in
          assert (fun_id = conf.cur_fun) ;
          let x_deps = VarSet.collect_e e in
          (* Simplify x_deps by looking up each one in the map *)
          let cur_vars =
            VarSet.fold
              (fun x acc -> VarSet.union (Env.get_or x deps ~default:VarSet.empty) acc)
              x_deps VarSet.empty in
          let deps = Env.add x cur_vars deps in
          let frame = { frame with deps; cur_vars } in
          stack <- frame :: rest ;
          Printf.printf "%s: %s\n" x (String.concat "," @@ VarSet.elements cur_vars)
      | Call _ ->
          (* arguments are not dependencies *)
          (* TODO: later, we'll want special handling of function calls *)
          ()

    method! record_subset1_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression option * value option)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      (match se2 with
      | Some se ->
          state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state
      | None -> ()) ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state

    method! record_subset2_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se2) state ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state

    method! record_if
        (conf : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state

    method! record_for
        (conf : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se) state

    method! dump_table : unit =
      (* let dump_function f ({ constrs; deps } : ConstraintNotNA.local_state) = *)
      (*   Printf.printf "function %s\n" f ; *)
      (*   Stdlib.print_endline "\tMust not be NA:" ; *)
      (*   Env.iter *)
      (*     (fun x c -> if c = ConstraintNotNA.Must_not_be_NA then Printf.printf "\t\t%s\n" x) *)
      (*     constrs ; *)
      (*  *)
      (*   Stdlib.print_endline "\tDependencies:" ; *)
      (*   Env.iter *)
      (*     (fun id deps -> *)
      (*       let set_to_s s = VarSet.elements s |> String.concat ", " in *)
      (*       Printf.printf "\t\t%s: %s\n" id (set_to_s deps)) *)
      (*     deps in *)
      (*  *)
      (* Stdlib.print_endline ">>> InferSpec <<<" ; *)
      (* FunTab.iter dump_function state *)
      ()
  end

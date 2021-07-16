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

(* fun_id: the current function's id
   deps: current map of variables to their (transitive) dependencies
   cur_deps: current dependencies of the most recently seen variables *)
type stack_frame =
  { fun_id : identifier [@default Common.main_function]
  ; deps : VarSet.t Env.t [@default Env.empty]
  ; cur_deps : VarSet.t [@default VarSet.empty]
  }
[@@deriving make]

class monitor =
  object (self)
    inherit Monitor.monitor

    val mutable state : ConstraintNotNA.t = FunTab.empty

    val mutable stack : stack_frame list = [ make_stack_frame () ]

    val mutable last_popped_frame : stack_frame option = None

    method private debug_print str =
      let n = List.length stack - 1 in
      Stdlib.print_string (String.make (n * 2) ' ') ;
      Stdlib.print_endline str

    (* Push a frame onto the shadow stack, and clear last_popped_frame. *)
    method private push_stack frame =
      stack <- frame :: stack ;
      last_popped_frame <- None

    (* Pop a frame from the shadow stack, set last_popped_frame, and return the frame. *)
    method private pop_stack =
      assert (List.length stack > 0) ;
      let top, rest = (List.hd stack, List.tl stack) in
      stack <- rest ;
      last_popped_frame <- Some top ;
      top

    method private update_cur_deps vars =
      let ({ deps; _ } as frame) = self#pop_stack in
      (* Compute the params that x depends on (transitively, via the intermediate deps) *)
      let param_deps =
        let get_deps_for x = Env.get_or x deps ~default:VarSet.empty in
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) vars VarSet.empty in
      self#push_stack { frame with cur_deps = param_deps } ;

      if VarSet.is_empty param_deps && VarSet.is_empty vars then
        self#debug_print @@ "Observed no vars, only constants"
      else if VarSet.is_empty param_deps then
        self#debug_print @@ "Vars " ^ VarSet.to_string vars ^ " are constants"
      else
        self#debug_print @@ "Observed " ^ VarSet.to_string vars ^ " (depends on: "
        ^ VarSet.to_string param_deps ^ ")"

    (* x depends on intermediates, but we want the params that x depends on.
       Update the deps map with x's dependencies. *)
    method private update_summary x intermediates =
      let ({ deps; _ } as frame) = self#pop_stack in

      (* Compute the params that x depends on (transitively, via the intermediate deps) *)
      let param_deps =
        let get_deps_for x = Env.get_or x deps ~default:VarSet.empty in
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) intermediates VarSet.empty
      in

      (* Update deps with x's dependencies, param_deps.
         This is a strong update, overwriting any previous dependencies of x.
         Update the stack frame with the new deps map, and current deps *)
      self#push_stack { frame with deps = Env.add x param_deps deps; cur_deps = param_deps } ;

      if VarSet.is_empty param_deps && VarSet.is_empty intermediates then
        self#debug_print @@ x ^ ": (none)"
      else if VarSet.is_empty param_deps then
        self#debug_print @@ x ^ ": (none, via " ^ VarSet.to_string intermediates ^ ")"
      else
        self#debug_print @@ x ^ ": " ^ VarSet.to_string param_deps ^ " (via "
        ^ VarSet.to_string intermediates ^ ")"

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

    (* Entering a function call, so we need to initialize a frame and push onto the shadow stack. *)
    method! record_call_entry
        (conf : configuration) (fun_id : identifier) (_ : simple_expression list * value list)
        : unit =
      (* Initialize deps so that each param maps to a singleton set containing itself,
         e.g. x -> {x}, y -> {y} *)
      let deps =
        let map_self map x = Env.add x (VarSet.singleton x) map in
        let params = Stdlib.fst @@ FunTab.find fun_id conf.fun_tab in
        List.fold_left map_self Env.empty params in
      self#debug_print @@ "--> Entering " ^ fun_id ;
      self#push_stack @@ make_stack_frame ~fun_id ~deps ()

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      let top = self#pop_stack in
      self#debug_print @@ "<-- Exiting " ^ top.fun_id ;
      self#debug_print @@ top.fun_id ^ "'s result depended on params "
      ^ VarSet.to_string top.cur_deps ;
      assert (top.fun_id = fun_id)

    (* Assigning a value to some variable x, so update the variables that x depends on.
       Specifically, we want to transitively follow the dependencies, to compute which params x depends on. *)
    method! record_assign (conf : configuration) (x : identifier) ((e, _) : expression * value)
        : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_e e) state ;
          self#update_summary x (VarSet.collect_e e)
      | Call (f, ses) ->
          assert (Option.is_some last_popped_frame) ;
          let { cur_deps; _ } = Option.get last_popped_frame in
          (* Create a mapping of f's params to argument variables *)
          let mapping =
            let params = Stdlib.fst @@ FunTab.find f conf.fun_tab in
            let arg_vars = List.map VarSet.collect_se ses in
            List.fold_left2 (fun acc p a -> Env.add p a acc) Env.empty params arg_vars in
          (* x depends on cur_deps depends on whatever mapping says *)
          let intermediates =
            VarSet.fold
              (fun x acc -> VarSet.union (Env.get_or x mapping ~default:VarSet.empty) acc)
              cur_deps VarSet.empty in
          self#update_summary x intermediates

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
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state ;
      self#update_summary x (VarSet.collect_se se3)

    method! record_subset2_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se2) state ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state ;
      self#update_summary x (VarSet.collect_se se3)

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

    method! record_expr_stmt (conf : configuration) ((e, _) : expression * value) : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          self#update_cur_deps (VarSet.collect_e e)
      | Call (f, ses) ->
          (* Current dependencies are dependencies of whatever f is returning *)
          assert (Option.is_some last_popped_frame) ;
          let { cur_deps; _ } = Option.get last_popped_frame in
          (* Create a mapping of f's params to argument variables *)
          let mapping =
            let params = Stdlib.fst @@ FunTab.find f conf.fun_tab in
            let arg_vars = List.map VarSet.collect_se ses in
            List.fold_left2 (fun acc p a -> Env.add p a acc) Env.empty params arg_vars in
          (* x depends on cur_deps depends on whatever mapping says *)
          let intermediates =
            VarSet.fold
              (fun x acc -> VarSet.union (Env.get_or x mapping ~default:VarSet.empty) acc)
              cur_deps VarSet.empty in
          self#update_cur_deps intermediates

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

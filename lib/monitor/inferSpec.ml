open Expr
open Common
open Util

let debug = true

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

type constr =
  | May_be_NA
  | Must_not_be_NA
[@@deriving show { with_path = false }]

(* Pair of (parameter list, constraint map) *)
type signature = Identifier.t list * constr Env.t

(* fun_id: the current function's id
   deps: current map of variables to their (transitive) dependencies
   cur_deps: current dependencies of the most recently seen variables
   constraints: constraints we've inferred on the current function's parameters *)
type stack_frame =
  { fun_id : identifier [@default Common.main_function]
  ; deps : VarSet.t Env.t [@default Env.empty]
  ; cur_deps : VarSet.t [@default VarSet.empty]
  ; constraints : constr Env.t [@default Env.empty]
  }
[@@deriving make]

class monitor =
  object (self)
    inherit Monitor.monitor

    (******************************************************************************
     * Internal monitor state
     ******************************************************************************)

    val mutable state : ConstraintNotNA.t = FunTab.empty

    val mutable constraints : signature FunTab.t = FunTab.empty

    val mutable stack : stack_frame list = [ make_stack_frame () ]

    val mutable last_popped_frame : stack_frame option = None

    (******************************************************************************
     * Monitor helpers
     ******************************************************************************)

    (* Debug printing, with indenting to reflect the stack depth. *)
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

    (* Update the summary of the current call.
       x, if present, is the variable being assigned to.
       vars are the (intermediate) variables involved.
       Therefore, x depends on vars, but we want to compute the transitive dependencies.
       If x is not present, we still want the transitive dependencies of the last seen vars. *)
    method private update_summary ?(is_strong = false) ?(x = None) vars =
      let ({ deps; _ } as frame) = self#pop_stack in

      (* Follow x's dependencies to compute the transitive dependencies. *)
      let get_deps_for x = Env.get_or x deps ~default:VarSet.empty in
      let new_deps =
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) vars VarSet.empty in

      (* Update deps, but only if we were updating some variable x. *)
      let deps' =
        match x with
        | None -> deps
        | Some x ->
            let to_add =
              (* On a strong update, overwrite old deps; otherwise union the old and new deps. *)
              if is_strong then new_deps else VarSet.union (get_deps_for x) new_deps in
            Env.add x to_add deps in
      self#push_stack { frame with deps = deps'; cur_deps = new_deps } ;

      if debug then
        let vars_str = if VarSet.is_empty vars then "<const>" else VarSet.to_string vars in
        match x with
        | None ->
            let new_deps_str =
              if VarSet.is_empty new_deps then "<none>" else VarSet.to_string new_deps in
            self#debug_print @@ new_deps_str ^ " (via: " ^ vars_str ^ ")"
        | Some x ->
            let current_deps = Env.get_or x deps' ~default:VarSet.empty in
            let current_deps_str =
              if VarSet.is_empty current_deps then "<none>" else VarSet.to_string current_deps in
            let weak_str = if is_strong then "" else " [weak update]" in
            self#debug_print @@ x ^ ": " ^ current_deps_str ^ " (via: " ^ vars_str ^ ")" ^ weak_str

    (* Calls need some extra processing before we can update the summary.
       At this point, we've already analyzed the call and popped the stack.
       Now we need to stitch the dependencies:
        - x (variable being assigned to, if present), to
        - variables in the call's return expression, to
        - the parameter dependencies, to
        - the call arguments.
       Essentially, we are creating a temporary transfer function for the call. *)
    method private update_summary_for_call ?(x = None) params arg_vars =
      assert (Option.is_some last_popped_frame) ;
      let { cur_deps; _ } = Option.get last_popped_frame in

      (* Create a mapping of params to argument variables *)
      let mapping = List.fold_left2 (fun acc p a -> Env.add p a acc) Env.empty params arg_vars in

      (* x depends on cur_deps depends on whatever mapping says *)
      let vars =
        let get_deps_for x = Env.get_or x mapping ~default:VarSet.empty in
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) cur_deps VarSet.empty in

      (* Always strong, because we can't have a function call in a subset assignment. *)
      self#update_summary ~is_strong:true ~x vars

    method private merge_constraints fun_id new_constraints =
      (* TODO: Have a real merge strategy that isn't "overwrite" *)
      let params, _ = FunTab.find fun_id constraints in
      FunTab.add fun_id (params, new_constraints) constraints

    method private add_constraints vars =
      let ({ deps; constraints; _ } as frame) = self#pop_stack in
      let param_vars =
        let get_deps_for x = Env.get_or x deps ~default:VarSet.empty in
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) vars VarSet.empty in
      let new_constraints =
        VarSet.fold (fun x acc -> FunTab.add x Must_not_be_NA acc) param_vars constraints in
      self#push_stack { frame with constraints = new_constraints } ;
      if debug then
        let param_vars_str =
          if VarSet.is_empty param_vars then "<none>" else VarSet.to_string param_vars in
        self#debug_print @@ ">> Must not be NA: " ^ param_vars_str ^ " (via: "
        ^ VarSet.to_string vars ^ ")"

    (******************************************************************************
     * Callbacks to record interpreter operations
     ******************************************************************************)

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
          state <- ConstraintNotNA.add_constraints conf.cur_fun vars state ;
          self#add_constraints vars
      | Arithmetic _ | Relational _ | Logical _ -> ()

    (* In a subset2 expression x[[i]], i must not be NA. *)
    method! record_subset2
        (conf : configuration)
        (_ : simple_expression * value)
        ((se, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state ;
      self#add_constraints (VarSet.collect_se se)

    (* Entering a function call, so we need to initialize some things. *)
    method! record_call_entry
        (conf : configuration) (fun_id : identifier) (_ : simple_expression list * value list)
        : unit =
      (* Initialize a stack frame and push onto the shadow stack.
         Set each param as a self-dependency. *)
      let params = Stdlib.fst @@ FunTab.find fun_id conf.fun_tab in
      let deps =
        let map_self map x = Env.add x (VarSet.singleton x) map in
        List.fold_left map_self Env.empty params in
      (* Initialize the constraint signature for this function.
         All params "may be NA" at this point. *)
      let param_constraints =
        List.fold_left (fun acc p -> Env.add p May_be_NA acc) Env.empty params in
      if debug then self#debug_print @@ "--> Entering " ^ fun_id ;
      self#push_stack @@ make_stack_frame ~fun_id ~deps ~constraints:param_constraints ()

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      (* Merge the constraints from the popped frame with the current state. *)
      let { fun_id = popped_fun; cur_deps; constraints = new_constraints; _ } = self#pop_stack in
      constraints <- self#merge_constraints fun_id new_constraints ;
      assert (popped_fun = fun_id) ;
      if debug then (
        self#debug_print @@ "<-- Exiting " ^ popped_fun ;
        self#debug_print @@ popped_fun ^ "'s result depended on params " ^ VarSet.to_string cur_deps)

    (* Assigning a value to some variable x, so update the variables that x depends on.
       Specifically, we want to transitively follow the dependencies, to compute which params x
       depends on. *)
    method! record_assign (conf : configuration) (x : identifier) ((e, _) : expression * value)
        : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_e e) state ;
          (* Assignment (non-subset) is always a strong update. *)
          self#update_summary ~is_strong:true ~x:(Some x) (VarSet.collect_e e)
      | Call (f, ses) ->
          if f = ".input" && List.length ses = 1 then (
            (* Treat the target variable as an input, so it is a self-dependency. *)
            let ({ deps; _ } as frame) = self#pop_stack in
            self#push_stack { frame with deps = Env.add x (VarSet.singleton x) deps } ;
            self#debug_print @@ x ^ " (input)")
          else
            let params = Stdlib.fst @@ FunTab.find f conf.fun_tab in
            let arg_vars = List.map VarSet.collect_se ses in
            self#update_summary_for_call ~x:(Some x) params arg_vars

    (* In a subset1 assignment x[i] <- v, i must not be NA. *)
    method! record_subset1_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression option * value option)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      (match se2 with
      | Some se ->
          state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state ;
          self#add_constraints (VarSet.collect_se se)
      | None -> ()) ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state ;
      (* Strong update if there is no index, i.e. assigning to entire vector. *)
      let is_strong = Option.is_none se2 in
      self#update_summary ~is_strong ~x:(Some x) (VarSet.collect_se se3)

    (* In a subset2 assignment x[[i]] <- v, i must not be NA. *)
    method! record_subset2_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se2) state ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state ;
      self#add_constraints (VarSet.collect_se se2) ;
      self#update_summary ~x:(Some x) (VarSet.collect_se se3)

    (* In an if statement, the condition cannot be NA. *)
    method! record_if
        (conf : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_constraints conf.cur_fun (VarSet.collect_se se) state ;
      self#add_constraints (VarSet.collect_se se)

    method! record_for
        (conf : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se) state ;
      self#update_summary ~is_strong:true ~x:(Some x) (VarSet.collect_se se)

    (* Function definition, so let's initialize the constraints map. *)
    method! record_fun_def
        (_ : configuration) (fun_id : identifier) (params : identifier list) (_ : statement list)
        : unit =
      let param_constraints =
        List.fold_left (fun acc p -> Env.add p May_be_NA acc) Env.empty params in
      let new_sig = (params, param_constraints) in
      constraints <- FunTab.add fun_id new_sig constraints

    (* This is an expression statement, so it may an expression returned by a function call.
       So we need to update the cur_deps to be the dependencies of the last variables seen. *)
    method! record_expr_stmt (conf : configuration) ((e, _) : expression * value) : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          self#update_summary (VarSet.collect_e e)
      | Call (f, ses) ->
          let params = Stdlib.fst @@ FunTab.find f conf.fun_tab in
          let arg_vars = List.map VarSet.collect_se ses in
          self#update_summary_for_call params arg_vars

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
      Stdlib.print_endline ">>> InferSpec <<<" ;
      let dump_function f (params, param_constrs) =
        let param_str = String.concat ", " params in
        Printf.printf "function %s(%s):\n" f param_str ;
        List.iter
          (fun p ->
            let c = Env.find p param_constrs in
            Printf.printf "\t%s: %s\n" p (show_constr c))
          params in
      FunTab.iter dump_function constraints
  end

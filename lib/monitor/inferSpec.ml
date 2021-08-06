open Expr
open Common
open Util

let debug = true

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

    (* Need to handle calling a function. At this point, we've already analyzed the call and popped
       the stack.
       1. Update summary:
            Calls need some extra processing before we can update the summary.
            Now we need to stich the dependencies:
              - x (variables being assigned to, if present), to
              - variables in the call's return expression, to
              - the parameter dependencies, to
              - the call arguments.
           Essentially, we are creating a temporary transfer function for the call.
       2. Propagate constraints *)
    method private update_summary_and_constraints_for_call ?(x = None) fun_tab f ses =
      assert (Option.is_some last_popped_frame) ;
      let { cur_deps; constraints; _ } = Option.get last_popped_frame in

      (* 1. Update the summary: x depends on some (or none) of the arguments of the call to f. *)

      (* Helper that returns the (caller) variable that x depends on.
         Uses a mapping of (callee) params to (callee) args (i.e. caller local vars). *)
      let get_deps_for x =
        let params = Stdlib.fst @@ FunTab.find f fun_tab in
        let args = List.map VarSet.collect_se ses in
        let mapping = List.fold_left2 (fun acc p a -> Env.add p a acc) Env.empty params args in
        Env.get_or x mapping ~default:VarSet.empty in

      (* (caller) x depends on (callee) cur_deps, which depends on whatever the mapping says. *)
      let vars =
        VarSet.fold (fun x acc -> VarSet.union (get_deps_for x) acc) cur_deps VarSet.empty in

      (* Always a strong update, because we can't have a function call in a subset assignment. *)
      self#update_summary ~is_strong:true ~x vars ;

      (* 2. Add constraints: constraints on f's params should apply to the call site arguments. *)

      (* Note: we want to use the constraints from the most recent invocation, not constraints from
         the global merged state. *)
      let param_constraints = Env.filter (fun _ c -> c = Must_not_be_NA) constraints in
      let arg_vars =
        Env.fold (fun x _ acc -> VarSet.union (get_deps_for x) acc) param_constraints VarSet.empty
      in
      self#add_constraints arg_vars

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
        (_ : configuration)
        (op : binary_op)
        ((se1, _) : simple_expression * value)
        ((se2, _) : simple_expression * value)
        (_ : value) : unit =
      match op with
      | Seq ->
          let vars = VarSet.union (VarSet.collect_se se1) (VarSet.collect_se se2) in
          self#add_constraints vars
      | Arithmetic _ | Relational _ | Logical _ -> ()

    (* In a subset2 expression x[[i]], i must not be NA. *)
    method! record_subset2
        (_ : configuration)
        (_ : simple_expression * value)
        ((se, _) : simple_expression * value)
        (_ : value) : unit =
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
          (* Assignment (non-subset) is always a strong update. *)
          self#update_summary ~is_strong:true ~x:(Some x) (VarSet.collect_e e)
      | Call (".input", ses) when List.length ses = 1 ->
          (* Treat the target variable as an input, so it is a self-dependency. *)
          let ({ deps; _ } as frame) = self#pop_stack in
          self#push_stack { frame with deps = Env.add x (VarSet.singleton x) deps } ;
          self#debug_print @@ x ^ " (input)"
      | Call (f, ses) -> self#update_summary_and_constraints_for_call conf.fun_tab f ses

    (* In a subset1 assignment x[i] <- v, i must not be NA. *)
    method! record_subset1_assign
        (_ : configuration)
        (x : identifier)
        ((se2, _) : simple_expression option * value option)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      (match se2 with
      | Some se -> self#add_constraints (VarSet.collect_se se)
      | None -> ()) ;
      (* Strong update if there is no index, i.e. assigning to entire vector. *)
      let is_strong = Option.is_none se2 in
      self#update_summary ~is_strong ~x:(Some x) (VarSet.collect_se se3)

    (* In a subset2 assignment x[[i]] <- v, i must not be NA. *)
    method! record_subset2_assign
        (_ : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      self#add_constraints (VarSet.collect_se se2) ;
      self#update_summary ~x:(Some x) (VarSet.collect_se se3)

    (* In an if statement, the condition cannot be NA. *)
    method! record_if
        (_ : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      self#add_constraints (VarSet.collect_se se)

    method! record_for
        (_ : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
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
      | Call (f, ses) -> self#update_summary_and_constraints_for_call conf.fun_tab f ses

    method! dump_table : unit =
      Stdlib.print_endline ">>> InferSpec <<<" ;
      let dump_function f (params, param_constrs) =
        let param_str = String.concat ", " params in
        Printf.printf "function %s(%s):\n" f param_str ;
        Printf.printf "    Parameters:\n" ;
        List.iter
          (fun p ->
            let c = Env.find p param_constrs in
            Printf.printf "        %s: %s\n" p (show_constr c))
          params in
      FunTab.iter dump_function constraints
  end

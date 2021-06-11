open Expr
open Common
open Util

(* TODO:
  - scope constraints to function (DONE but need to refactor)
  - function-level constraints?
  - separate models for separate function calls, e.g. call-site sensitivity

  - other constraints (e.g. types)
  - better abstraction/modularity for constraints
    - how to run with multiple constraints
*)

module ConstraintNotNA = struct
  type constraints = VarSet.t
  type dependencies = VarSet.t Env.t
  type t =
    { c : constraints
    ; d : dependencies
    }

  let empty = { c = VarSet.empty; d = Env.empty }

  let add_constraint (xs : VarSet.t) (constraints : constraints) = VarSet.union xs constraints

  let add_deps (x : identifier) (deps : VarSet.t) (dep_map : dependencies) =
    match Env.find_opt x dep_map with
    | Some old -> Env.add x (VarSet.union old deps) dep_map
    | None -> Env.add x deps dep_map

  let rec propagate (deps : dependencies) (constraints : constraints) =
    let new_constraints =
      constraints |> VarSet.elements
      |> List.map (fun v -> Env.get_or v deps ~default:VarSet.empty)
      |> List.fold_left (fun acc vs -> VarSet.union acc vs) constraints in
    if new_constraints = constraints then constraints else propagate deps new_constraints
end

type abstract_state = ConstraintNotNA.t FunTab.t

class monitor =
  object
    inherit Monitor.monitor

    val mutable state : abstract_state = FunTab.empty

    method! record_binary_op
        (conf : configuration)
        (op : binary_op)
        ((se1, _) : simple_expression * value)
        ((se2, _) : simple_expression * value)
        (_ : value) : unit =
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      match op with
      | Seq ->
          let vars = VarSet.union (VarSet.collect_se se1) (VarSet.collect_se se2) in
          let constraints' =
            constraints |> ConstraintNotNA.add_constraint vars |> ConstraintNotNA.propagate deps
          in
          state <- Env.add conf.cur_fun { fun_state with c = constraints' } state
      | Arithmetic _ | Relational _ | Logical _ -> ()

    method! record_subset2
        (conf : configuration)
        (_ : simple_expression * value)
        ((se, _) : simple_expression * value)
        (_ : value) : unit =
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      let constraints' =
        constraints
        |> ConstraintNotNA.add_constraint (VarSet.collect_se se)
        |> ConstraintNotNA.propagate deps in
      state <- Env.add conf.cur_fun { fun_state with c = constraints' } state

    method! record_assign (conf : configuration) (x : identifier) ((e, _) : expression * value)
        : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
          let constraints, deps = (fun_state.c, fun_state.d) in
          let deps' = ConstraintNotNA.add_deps x (VarSet.collect_e e) deps in
          let constraints' = ConstraintNotNA.propagate deps' constraints in
          let fun_state' : ConstraintNotNA.t = { c = constraints'; d = deps' } in
          state <- Env.add conf.cur_fun fun_state' state
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
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      (* se2 cannot be NA *)
      let constraints' =
        match se2 with
        | Some se -> ConstraintNotNA.add_constraint (VarSet.collect_se se) constraints
        | None -> constraints in
      let deps' = ConstraintNotNA.add_deps x (VarSet.collect_se se3) deps in
      let constraints' = ConstraintNotNA.propagate deps' constraints' in
      let fun_state' : ConstraintNotNA.t = { c = constraints'; d = deps' } in
      state <- Env.add conf.cur_fun fun_state' state

    method! record_subset2_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      (* se2 cannot be NA *)
      let constraints' = ConstraintNotNA.add_constraint (VarSet.collect_se se2) constraints in
      let deps' = ConstraintNotNA.add_deps x (VarSet.collect_se se3) deps in
      let constraints' = ConstraintNotNA.propagate deps' constraints' in
      let fun_state' : ConstraintNotNA.t = { c = constraints'; d = deps' } in
      state <- Env.add conf.cur_fun fun_state' state

    method! record_if
        (conf : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      let constraints' =
        constraints
        |> ConstraintNotNA.add_constraint (VarSet.collect_se se)
        |> ConstraintNotNA.propagate deps in
      state <- Env.add conf.cur_fun { fun_state with c = constraints' } state

    method! record_for
        (conf : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      let fun_state = FunTab.get_or conf.cur_fun state ~default:ConstraintNotNA.empty in
      let constraints, deps = (fun_state.c, fun_state.d) in
      let deps' = ConstraintNotNA.add_deps x (VarSet.collect_se se) deps in
      let constraints' = ConstraintNotNA.propagate deps' constraints in
      let fun_state' : ConstraintNotNA.t = { c = constraints'; d = deps' } in
      state <- Env.add conf.cur_fun fun_state' state

    method! dump_table : unit =
      let dump_function f ({ c; d } : ConstraintNotNA.t) =
        Printf.printf "function %s\n" f ;
        Stdlib.print_endline "\tMust not be NA:" ;
        VarSet.iter (fun x -> Printf.printf "\t\t%s\n" x) c ;

        Stdlib.print_endline "\tDependencies:" ;
        Env.iter
          (fun id deps ->
            let set_to_s s = VarSet.elements s |> String.concat ", " in
            Printf.printf "\t\t%s: %s\n" id (set_to_s deps))
          d in

      Stdlib.print_endline ">>> InferSpec <<<" ;
      FunTab.iter dump_function state
  end

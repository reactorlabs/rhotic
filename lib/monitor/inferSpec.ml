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
  type constraints = VarSet.t
  type dependencies = VarSet.t Env.t
  type local_state =
    { constrs : constraints
    ; deps : dependencies
    }
  type t = local_state FunTab.t

  let empty = { constrs = VarSet.empty; deps = Env.empty }

  let rec propagate (deps : dependencies) (constraints : constraints) =
    let new_constraints =
      constraints |> VarSet.elements
      |> List.map (fun v -> Env.get_or v deps ~default:VarSet.empty)
      |> List.fold_left (fun acc vs -> VarSet.union acc vs) constraints in
    if new_constraints = constraints then constraints else propagate deps new_constraints

  let add_constraint (cur_fun : identifier) (xs : VarSet.t) (state : t) =
    let { constrs; deps } = FunTab.get_or cur_fun state ~default:empty in
    let constrs = VarSet.union xs constrs |> propagate deps in
    Env.add cur_fun { constrs; deps } state

  let add_deps (cur_fun : identifier) (x : identifier) (vars : VarSet.t) (state : t) =
    let { constrs; deps } = FunTab.get_or cur_fun state ~default:empty in
    let old_vars = Env.get_or x deps ~default:VarSet.empty in
    let deps = Env.add x (VarSet.union old_vars vars) deps in
    let constrs = propagate deps constrs in
    Env.add cur_fun { constrs; deps } state
end

class monitor =
  object
    inherit Monitor.monitor

    val mutable state : ConstraintNotNA.t = FunTab.empty

    method! record_binary_op
        (conf : configuration)
        (op : binary_op)
        ((se1, _) : simple_expression * value)
        ((se2, _) : simple_expression * value)
        (_ : value) : unit =
      match op with
      | Seq ->
          let vars = VarSet.union (VarSet.collect_se se1) (VarSet.collect_se se2) in
          state <- ConstraintNotNA.add_constraint conf.cur_fun vars state
      | Arithmetic _ | Relational _ | Logical _ -> ()

    method! record_subset2
        (conf : configuration)
        (_ : simple_expression * value)
        ((se, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraint conf.cur_fun (VarSet.collect_se se) state

    method! record_assign (conf : configuration) (x : identifier) ((e, _) : expression * value)
        : unit =
      match e with
      | Combine _ | Dataframe_Ctor _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _
      | Simple_Expression _ ->
          state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_e e) state
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
      | Some se -> state <- ConstraintNotNA.add_constraint conf.cur_fun (VarSet.collect_se se) state
      | None -> ()) ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state

    method! record_subset2_assign
        (conf : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      state <- ConstraintNotNA.add_constraint conf.cur_fun (VarSet.collect_se se2) state ;
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se3) state

    method! record_if
        (conf : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_constraint conf.cur_fun (VarSet.collect_se se) state

    method! record_for
        (conf : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      state <- ConstraintNotNA.add_deps conf.cur_fun x (VarSet.collect_se se) state

    method! dump_table : unit =
      let dump_function f ({ constrs; deps } : ConstraintNotNA.local_state) =
        Printf.printf "function %s\n" f ;
        Stdlib.print_endline "\tMust not be NA:" ;
        VarSet.iter (fun x -> Printf.printf "\t\t%s\n" x) constrs ;

        Stdlib.print_endline "\tDependencies:" ;
        Env.iter
          (fun id deps ->
            let set_to_s s = VarSet.elements s |> String.concat ", " in
            Printf.printf "\t\t%s: %s\n" id (set_to_s deps))
          deps in

      Stdlib.print_endline ">>> InferSpec <<<" ;
      FunTab.iter dump_function state
  end

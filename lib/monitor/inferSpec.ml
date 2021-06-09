open Expr
open Common
open Util

(* TODO: More places where NA not allowed:
  - seq
  - subset2 index
*)

(* TODO:
  - scope constraints to function
  - other constraints (e.g. types)
  - better abstraction/modularity for constraints
    - how to run with multiple constraints
*)

module ConstraintNotNA = struct
  type c = VarSet.t
  type d = VarSet.t Env.t

  let gen_constraint xs constraints = VarSet.union xs constraints

  let add_deps x deps dep_map =
    match Env.find_opt x dep_map with
    | Some old -> Env.add x (VarSet.union old deps) dep_map
    | None -> Env.add x deps dep_map

  let rec propagate deps constraints =
    let new_constraints =
      constraints |> VarSet.elements
      |> List.map (fun v -> Env.get_or v deps ~default:VarSet.empty)
      |> List.fold_left (fun acc vs -> VarSet.union acc vs) constraints in
    if new_constraints = constraints then constraints else propagate deps new_constraints
end

class monitor =
  object
    inherit Monitor.monitor

    val mutable var_dependencies = Env.empty
    val mutable must_not_be_na = VarSet.empty

    method! record_assign (_ : configuration) (x : identifier) ((e, _) : expression * value) : unit
        =
      var_dependencies <- ConstraintNotNA.add_deps x (VarSet.collect_e e) var_dependencies ;
      must_not_be_na <- ConstraintNotNA.propagate var_dependencies must_not_be_na

    method! record_subset1_assign
        (_ : configuration)
        (x : identifier)
        ((se2, _) : simple_expression option * value option)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      (* se2 cannot be NA *)
      if Option.is_some se2 then
        must_not_be_na <-
          ConstraintNotNA.gen_constraint (VarSet.collect_se @@ Option.get se2) must_not_be_na ;
      var_dependencies <- ConstraintNotNA.add_deps x (VarSet.collect_se se3) var_dependencies ;
      must_not_be_na <- ConstraintNotNA.propagate var_dependencies must_not_be_na

    method! record_subset2_assign
        (_ : configuration)
        (x : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      (* se2 cannot be NA *)
      must_not_be_na <- ConstraintNotNA.gen_constraint (VarSet.collect_se se2) must_not_be_na ;
      var_dependencies <- ConstraintNotNA.add_deps x (VarSet.collect_se se3) var_dependencies ;
      must_not_be_na <- ConstraintNotNA.propagate var_dependencies must_not_be_na

    method! record_if
        (_ : configuration)
        ((se, _) : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      must_not_be_na <- ConstraintNotNA.gen_constraint (VarSet.collect_se se) must_not_be_na ;
      must_not_be_na <- ConstraintNotNA.propagate var_dependencies must_not_be_na

    method! record_for
        (_ : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      var_dependencies <- ConstraintNotNA.add_deps x (VarSet.collect_se se) var_dependencies ;
      must_not_be_na <- ConstraintNotNA.propagate var_dependencies must_not_be_na

    method! dump_table : unit =
      Stdlib.print_endline ">>> InferSpec <<<" ;
      Stdlib.print_endline "Must not be NA:" ;
      VarSet.iter (fun x -> Printf.printf "\t%s\n" x) must_not_be_na ;

      let f id deps =
        let set_to_s s = VarSet.elements s |> String.concat ", " in
        Printf.printf "\t%s: %s\n" id (set_to_s deps) in

      Stdlib.print_endline "Dependencies:" ;
      Env.iter f var_dependencies
  end

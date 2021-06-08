open Expr
open Common

(* TODO: More places where NA not allowed:
  - seq
  - subset2 index
*)

(* TODO:
  - scope constraints to function
  - other constraints (e.g. types)
  - better abstraction/modularity for constraints
    - there's duplicate code for adding constraints and var dependencies *)

let rec propagate_constraints var_deps constraints =
  let new_constraints =
    constraints |> VarSet.elements
    |> List.map (fun v -> if Env.mem v var_deps then Env.find v var_deps else VarSet.empty)
    |> List.fold_left (fun acc vs -> VarSet.union acc vs) constraints in
  if new_constraints = constraints then constraints
  else propagate_constraints var_deps new_constraints

class monitor =
  object
    inherit Monitor.monitor

    val mutable var_dependencies = Env.empty
    val mutable must_not_be_na = VarSet.empty

    method! record_assign (_ : configuration) (x : identifier) (e : expression) (_ : value) : unit =
      let deps = VarSet.collect_e e in
      (match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old deps) var_dependencies
      | None -> var_dependencies <- Env.add x deps var_dependencies) ;
      (* Propagate constraints *)
      must_not_be_na <- propagate_constraints var_dependencies must_not_be_na

    method! record_subset1_assign
        (_ : configuration)
        (x : identifier)
        (se2 : simple_expression option)
        (se3 : simple_expression)
        (_ : value option)
        (_ : value)
        (_ : value) : unit =
      (* se2 cannot be NA *)
      if Option.is_some se2 then
        must_not_be_na <- VarSet.union (VarSet.collect_se @@ Option.get se2) must_not_be_na ;
      (* Update dependencies for x *)
      let operands = VarSet.collect_se se3 in
      (match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old operands) var_dependencies
      | None -> var_dependencies <- Env.add x operands var_dependencies) ;
      (* Propagate constraints *)
      must_not_be_na <- propagate_constraints var_dependencies must_not_be_na

    method! record_subset2_assign
        (_ : configuration)
        (x : identifier)
        (se2 : simple_expression)
        (se3 : simple_expression)
        (_ : value)
        (_ : value)
        (_ : value) : unit =
      (* se2 cannot be NA *)
      must_not_be_na <- VarSet.union (VarSet.collect_se se2) must_not_be_na ;
      (* Update dependencies for x *)
      let operands = VarSet.collect_se se3 in
      (match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old operands) var_dependencies
      | None -> var_dependencies <- Env.add x operands var_dependencies) ;
      (* Propagate constraints *)
      must_not_be_na <- propagate_constraints var_dependencies must_not_be_na

    method! record_if
        (_ : configuration)
        (se : simple_expression)
        (_ : statement list)
        (_ : statement list)
        (_ : value) : unit =
      must_not_be_na <- VarSet.union (VarSet.collect_se se) must_not_be_na ;
      (* Propagate constraints *)
      must_not_be_na <- propagate_constraints var_dependencies must_not_be_na

    method! record_for
        (_ : configuration)
        (x : identifier)
        (se : simple_expression)
        (_ : statement list)
        (_ : value) : unit =
      let operands = VarSet.collect_se se in
      (match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old operands) var_dependencies
      | None -> var_dependencies <- Env.add x operands var_dependencies) ;
      (* Propagate constraints *)
      must_not_be_na <- propagate_constraints var_dependencies must_not_be_na

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

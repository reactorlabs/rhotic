open Expr
open Common

(* TODO subset1_assign, subset2_assign *)

class monitor =
  object
    inherit Monitor.monitor

    val mutable var_dependencies = Env.empty
    val mutable must_not_be_na = VarSet.empty

    method! record_assign (_ : configuration) (x : identifier) (e : expression) (_ : value) : unit =
      let deps = VarSet.collect_e e in
      match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old deps) var_dependencies
      | None -> var_dependencies <- Env.add x deps var_dependencies

    method! record_if
        (_ : configuration)
        (se : simple_expression)
        (_ : statement list)
        (_ : statement list)
        (_ : value) : unit =
      must_not_be_na <- VarSet.union (VarSet.collect_se se) must_not_be_na

    method! record_for
        (_ : configuration)
        (x : identifier)
        (se : simple_expression)
        (_ : statement list)
        (_ : value) : unit =
      let operands = VarSet.collect_se se in
      match Env.find_opt x var_dependencies with
      | Some old -> var_dependencies <- Env.add x (VarSet.union old operands) var_dependencies
      | None -> var_dependencies <- Env.add x operands var_dependencies

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

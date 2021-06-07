open Expr
open Common

(* TODO assign, subset1_assign, subset2_assign, for *)

class monitor =
  object
    inherit Monitor.monitor

    val mutable must_not_be_na = VarSet.empty

    method! record_if
        (_ : configuration)
        (se : simple_expression)
        (_ : statement list)
        (_ : statement list)
        (_ : value) : unit =
      must_not_be_na <- VarSet.union (VarSet.collect_se se) must_not_be_na

    method! dump_table : unit =
      Stdlib.print_endline ">>> InferSpec <<<" ;
      Stdlib.print_endline "Must not be NA:" ;
      VarSet.iter (fun x -> Printf.printf "\t%s\n" x) must_not_be_na

  end

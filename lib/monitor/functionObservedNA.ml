open Expr
open Common
open Util

type function_seen_nas = bool FunTab.t

let contains_na =
  vector_data
  %> Array.exists (function
       | NA_bool | NA_int | NA_str -> true
       | Bool _ | Int _ | Str _ -> false)

class monitor =
  object
    inherit Monitor.monitor

    val mutable table : function_seen_nas = FunTab.empty

    (* Update the entry for the called function.
       If this method is called, then the function must exist. *)
    method! record_call_exit
        (_ : configuration)
        (id : identifier)
        ((_, args) : simple_expression list * value list)
        (ret : value) : unit =
      let res = FunTab.find id table || contains_na ret || List.exists contains_na args in
      table <- FunTab.add id res table

    (* Create an empty entry, with the args list initialized to Bot *)
    method! record_fun_def
        (_ : configuration) (id : identifier) (_ : identifier list) (_ : statement list) : unit =
      table <- FunTab.add id false table

    method! dump_table : unit =
      Stdlib.print_endline ">>> FunctionObservedNA <<<" ;

      let f id observed =
        Printf.printf "\tObserved NAs for %s? " id ;
        if observed then Printf.printf "Yes\n" else Printf.printf "No\n" in
      FunTab.iter f table
  end

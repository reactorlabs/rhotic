open Expr
open Common

class virtual monitor =
  object
    method record_call (_ : configuration) (_ : value) (_ : identifier) (_ : value list) : unit = ()

    method record_fun_def (_ : configuration) (_ : identifier) (_ : identifier list) : unit = ()

    method dump_table : unit = ()
  end

type monitors = monitor list

let dump_state (monitors : monitors) = List.iter (fun m -> m#dump_table) monitors

exception Unknown_monitor of string

open Expr
open Common

class virtual monitor =
  object
    method record_call
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression list)
        (_ : value list)
        (_ : value) : unit =
      ()

    method record_assign (_ : configuration) (_ : identifier) (_ : expression) (_ : value) : unit =
      ()

    method record_subset1_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression option)
        (_ : simple_expression)
        (_ : value option)
        (_ : value)
        (_ : value) : unit =
      ()

    method record_subset2_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression)
        (_ : simple_expression)
        (_ : value)
        (_ : value)
        (_ : value) : unit =
      ()

    method record_if
        (_ : configuration)
        (_ : simple_expression)
        (_ : statement list)
        (_ : statement list)
        (_ : value) : unit =
      ()

    method record_for
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression)
        (_ : statement list)
        (_ : value) : unit =
      ()

    method record_fun_def
        (_ : configuration) (_ : identifier) (_ : identifier list) (_ : statement list) : unit =
      ()

    method record_expr_stmt (_ : configuration) (_ : expression) (_ : value) : unit = ()

    method dump_table : unit = ()
  end

type monitors = monitor list

let dump_state (monitors : monitors) = List.iter (fun m -> m#dump_table) monitors

exception Unknown_monitor of string

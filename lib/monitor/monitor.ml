open Containers
open Expr
open Common

class virtual monitor =
  object
    (******************************************************************************
     * Expressions
     ******************************************************************************)
    method record_combine (_ : configuration) (_ : simple_expression list * value list) (_ : value)
        : unit =
      ()

    method record_unary_op
        (_ : configuration) (_ : unary_op) (_ : simple_expression * value) (_ : value) : unit =
      ()

    method record_binary_op
        (_ : configuration)
        (_ : binary_op)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    method record_subset1
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : simple_expression option * value option)
        (_ : value) : unit =
      ()

    method record_subset2
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    method record_call_entry
        (_ : configuration) (_ : identifier) (_ : simple_expression list * value list) : unit =
      ()

    method record_call_exit
        (_ : configuration) (_ : identifier) (_ : simple_expression list * value list) (_ : value)
        : unit =
      ()

    method record_simple_expr (_ : configuration) (_ : simple_expression * value) : unit = ()

    (******************************************************************************
     * Statements
     ******************************************************************************)
    method record_assign (_ : configuration) (_ : identifier) (_ : expression * value) : unit = ()

    method record_subset1_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression option * value option)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    method record_subset2_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    method record_if_entry
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      ()

    method record_if_exit (_ : configuration) (_ : value) : unit = ()

    method record_for_entry
        (_ : configuration) (_ : identifier) (_ : simple_expression * value) (_ : statement list)
        : unit =
      ()

    method record_for_iteration
        (_ : configuration) (_ : identifier * value) (_ : value) (_ : statement list) : unit =
      ()

    method record_for_exit (_ : configuration) (_ : value) : unit = ()

    method record_fun_def
        (_ : configuration) (_ : identifier) (_ : identifier list) (_ : statement list) : unit =
      ()

    method record_print_stmt (_ : configuration) (_ : expression * value) : unit = ()

    method record_expr_stmt (_ : configuration) (_ : expression * value) : unit = ()

    (******************************************************************************
     * Everything else
     ******************************************************************************)
    method program_entry (_ : configuration) : unit = ()

    method program_exit (_ : configuration) : unit = ()

    method dump_table : unit = ()
  end

type monitors = monitor list

let dump_state (monitors : monitors) = List.iter (fun m -> m#dump_table) monitors

exception Unknown_monitor of string

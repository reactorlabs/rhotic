open Expr
open Common

class virtual monitor =
  object
    method record_call (_ : configuration) (_ : value) (_ : identifier) (_ : value list) : unit = ()

    method record_fun_def (_ : configuration) (_ : identifier) (_ : identifier list) : unit = ()

    method dump_table : unit = ()
  end

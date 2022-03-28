open Util
open Opcode

module type AnalysisInstance = sig
  type astate
  val init : pc -> opcode -> astate
  val show_op : astate -> string
  val show : astate -> string
  val leq : astate -> astate -> bool
  val merge : astate -> astate -> astate
  val step : astate -> astate
  val call : Expr.identifier list -> Expr.simple_expression list -> astate -> astate
  val return : Expr.identifier list -> astate -> astate
end

module type S = sig
  type astate
  val analyze : ?debug:bool -> opcode Vector.ro_vector * pc -> astate Vector.ro_vector
end

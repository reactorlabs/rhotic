open Util
open Opcode

module type AnalysisInstance = sig
  type astate
  val init : pc -> opcode -> astate
  val show_op : astate -> string
  val show : astate -> string
  val leq : astate -> astate -> bool
  val merge : astate -> astate -> astate
  val step : ?cstate:EvalState.t -> astate -> astate
  val call :
    Expr.identifier list -> Expr.simple_expression list -> ?cstate:EvalState.t -> astate -> astate
  val return : Expr.identifier list -> ?cstate:EvalState.t -> astate -> astate
end

module type S = sig
  type astate
  val analyze : ?debug:bool -> opcode Vector.ro_vector * pc -> astate Vector.ro_vector
end

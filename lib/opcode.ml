open Containers
open Expr

module Pc = struct
  type t = int
  let compare = Int.compare
  let equal = Int.equal
  let pp = Format.pp_print_int
end

type pc = Pc.t [@@deriving eq, show]

type builtin =
  | Unary          of unary_op [@printer fun fmt o -> fprintf fmt "`%s`" (show_unary_op o)]
  | Binary         of binary_op [@printer fun fmt o -> fprintf fmt "`%s`" (show_binary_op o)]
  | Combine [@printer fun fmt _ -> fprintf fmt "`c`"]
  | Input [@printer fun fmt _ -> fprintf fmt "`input`"]
  | Subset1 [@printer fun fmt _ -> fprintf fmt "`[`"]
  | Subset2 [@printer fun fmt _ -> fprintf fmt "`[[`"]
  | Subset1_Assign [@printer fun fmt _ -> fprintf fmt "`[<-`"]
  | Subset2_Assign [@printer fun fmt _ -> fprintf fmt "`[[<-`"]

and opcode =
  | Nop
  | Copy    of identifier * simple_expression
      [@printer fun fmt (id, se) -> fprintf fmt "%s = %s" id (show_simple_expression se)]
  | Call    of
      { target : identifier
      ; fn : identifier
      ; fn_pc : pc
      ; params : identifier list
      ; args_se : simple_expression list
      }
      [@printer
        fun fmt target fn fn_pc params args ->
          fprintf fmt "%s = %s(%s) ; L%d" target fn
            (List.map2 (fun p a -> p ^ "=" ^ show_simple_expression a) params args
            |> String.concat ",")
            fn_pc]
  | Builtin of identifier * builtin * simple_expression list
      [@printer
        fun fmt (target, builtin, args) ->
          fprintf fmt "%s = %s(%s)" target (show_builtin builtin)
            (args |> List.map show_simple_expression |> String.concat ",")]
  | Entry   of identifier * identifier list
      [@printer fun fmt (id, params) -> fprintf fmt "Entry %s(%s)" id (String.concat "," params)]
  | Exit    of identifier [@printer fun fmt -> fprintf fmt "Exit %s"]
  | Start
  | Stop
  | Jump    of pc [@printer fun fmt -> fprintf fmt "Jump L%d"]
  | Branch  of simple_expression * pc
      [@printer
        fun fmt (cond, target_pc) ->
          fprintf fmt "Branch %s L%d" (show_simple_expression cond) target_pc]
  | Print   of simple_expression
      (* Print is not a "builtin" because it is side-effecting and doesn't return a value. *)
      [@printer fun fmt se -> fprintf fmt "Print %s" (show_simple_expression se)]
  | Comment of string [@printer fun fmt -> fprintf fmt "; %s"]
[@@deriving eq, show { with_path = false }]

let builtin target builtin args = Builtin (target, builtin, args)
let copy target se = Copy (target, se)

let show_pc_opcode pc op =
  match[@warning "-4"] op with
  | Comment _ -> Printf.sprintf "%s" (show_opcode op)
  | _ -> Printf.sprintf "%4d\t%s" pc (show_opcode op)

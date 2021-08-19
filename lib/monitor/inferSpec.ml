open Expr
open Common
open Util

let debug = true

module Lattice = struct
  (*
          Top
         /   \
       NA   Not_NA
         \   /
          Bot
  *)
  type t =
    | Top
    | NA
    | Not_NA
    | Bot

  (* less than or equal is the ordering relation *)
  let leq x y =
    match (x, y) with
    | Bot, _ | _, Top | NA, NA | Not_NA, Not_NA -> true
    | _, Bot | Top, _ | NA, Not_NA | Not_NA, NA -> false

  (* least upper bound is join, an over-approximation *)
  let lub x y =
    match (x, y) with
    (* Bot and Top cases *)
    | Bot, z | z, Bot -> z
    | Top, _ | _, Top -> Top
    (* x = y cases *)
    | NA, NA -> NA
    | Not_NA, Not_NA -> Not_NA
    (* Everything else *)
    | NA, Not_NA | Not_NA, NA -> Top

  (* greatest lower bound is meet, an under-approximation *)
  let glb x y =
    match (x, y) with
    (* Bot and Top cases *)
    | Bot, _ | _, Bot -> Bot
    | Top, z | z, Top -> z
    (* x = y cases *)
    | NA, NA -> NA
    | Not_NA, Not_NA -> Not_NA
    (* Everything else *)
    | NA, Not_NA | Not_NA, NA -> Bot
end

type lattice = Lattice.t

(* abstract value *)
type avalue =
  { lower : lattice [@default Lattice.Bot]
  ; upper : lattice [@default Lattice.Top]
  }
[@@deriving make]

type stack_frame = { fun_id : identifier } [@@deriving make]

class monitor =
  object (self)
    inherit Monitor.monitor

    (******************************************************************************
     * Internal monitor state
     ******************************************************************************)

    val mutable stack_ : stack_frame list = []

    (******************************************************************************
     * Monitor helpers
     ******************************************************************************)

    (* Debug printing, with indenting to reflect the stack depth. *)
    method private debug_print str =
      let n = List.length stack_ - 1 in
      Stdlib.print_string (String.make (n * 2) ' ') ;
      Stdlib.print_endline str

    (* Push a frame onto the shadow stack, and clear last_popped_frame. *)
    method private push_stack frame = stack_ <- frame :: stack_

    (* Pop a frame from the shadow stack, set last_popped_frame, and return the frame. *)
    method private pop_stack =
      assert (List.length stack_ > 0) ;
      let top, rest = (List.hd stack_, List.tl stack_) in
      stack_ <- rest ;
      top

    (******************************************************************************
     * Callbacks to record interpreter operations
     ******************************************************************************)

    (* In a sequence expression x:y, x and y must not be NA. *)
    method! record_binary_op
        (_ : configuration)
        (_ : binary_op)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* In a subset2 expression x[[i]], i must not be NA. *)
    method! record_subset2
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* Entering a function call, so we need to initialize some things. *)
    method! record_call_entry
        (_ : configuration) (fun_id : identifier) (_ : simple_expression list * value list) : unit =
      (* Initialize a stack frame and push onto the shadow stack. *)
      if debug then self#debug_print @@ "--> Entering " ^ fun_id ;
      self#push_stack @@ make_stack_frame ~fun_id

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      (* Merge the constraints from the popped frame with the current state. *)
      let { fun_id = popped_fun; _ } = self#pop_stack in
      assert (popped_fun = fun_id) ;
      if debug then self#debug_print @@ "<-- Exiting " ^ popped_fun

    (* Assigning a value to some variable x, so update the variables that x depends on.
       Specifically, we want to transitively follow the dependencies, to compute which params x
       depends on. *)
    method! record_assign (_ : configuration) (_ : identifier) (_ : expression * value) : unit = ()

    (* In a subset1 assignment x[i] <- v, i must not be NA. *)
    method! record_subset1_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression option * value option)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* In a subset2 assignment x[[i]] <- v, i must not be NA. *)
    method! record_subset2_assign
        (_ : configuration)
        (_ : identifier)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* In an if statement, the condition cannot be NA. *)
    method! record_if
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      ()

    (* In a for statement, identifier depends on the value we're looping over. *)
    method! record_for
        (_ : configuration) (_ : identifier) (_ : simple_expression * value) (_ : statement list)
        : unit =
      ()

    (* Function definition, so let's initialize the constraints map. *)
    method! record_fun_def
        (_ : configuration) (_ : identifier) (_ : identifier list) (_ : statement list) : unit =
      ()

    (* This is an expression statement, so it may an expression returned by a function call. *)
    method! record_expr_stmt (_ : configuration) (_ : expression * value) : unit = ()

    method! program_entry ({ cur_fun = fun_id; _ } : configuration) : unit =
      self#push_stack @@ make_stack_frame ~fun_id

    method! program_exit (_ : configuration) : unit =
      let _ = self#pop_stack in
      assert (List.length stack_ = 0)

    method! dump_table : unit = ()
  end

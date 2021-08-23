open Containers
open Expr
open Common

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

  let equal x y = Equal.physical x y

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
type lattice = Lattice.t [@@deriving eq]

(* abstract value *)
type avalue =
  { lower : lattice [@default Lattice.Bot]
  ; upper : lattice [@default Lattice.Top]
  }
[@@deriving eq, make]

(* need some indirection: we store avalues in a pool because they are mutable *)
type avalue_id = int
type aenvironment = avalue_id Env.t

(* abstract stack frame *)
type astack_frame =
  { fun_id : identifier
  ; env : aenvironment [@default Env.empty]
  }
[@@deriving make]

class monitor =
  object (self)
    inherit Monitor.monitor

    (******************************************************************************
     * Internal monitor state
     ******************************************************************************)

    (* abstract stack for the program *)
    val mutable astack_ : astack_frame list = []

    (* abstract value pool, essentially an abstract heap *)
    val aval_pool_ : avalue Vector.vector = Vector.create ()

    (******************************************************************************
     * Monitor helpers
     ******************************************************************************)

    (* Debug printing, with indenting to reflect the stack depth. *)
    method private debug_print (str : string) : unit =
      let n = List.length astack_ - 1 in
      if n > 0 then Stdlib.print_string (String.make (n * 2) ' ') ;
      Stdlib.print_endline str

    (* Push a frame onto the shadow stack *)
    method private push_stack (frame : astack_frame) : unit = astack_ <- frame :: astack_

    (* Pop a frame from the shadow stack, and return the frame. *)
    method private pop_stack : astack_frame =
      assert (List.length astack_ > 0) ;
      match astack_ with
      | [] -> assert false
      | top :: rest ->
          astack_ <- rest ;
          top

    (* Add an avalue to the pool, and return its id. *)
    method private push_avalue (aval : avalue) : avalue_id =
      let id = Vector.length aval_pool_ in
      Vector.push aval_pool_ aval ;
      assert (equal_avalue (Vector.get aval_pool_ id) aval) ;
      id

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
      if debug then self#debug_print @@ "--> Entering " ^ fun_id ;
      (* Initialize a stack frame and push onto the shadow stack. *)
      self#push_stack @@ make_astack_frame ~fun_id ()

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      let { fun_id = popped_fun; _ } = self#pop_stack in
      assert (Identifier.equal popped_fun fun_id) ;
      if debug then self#debug_print @@ "<-- Exiting " ^ popped_fun

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

    method! record_for
        (_ : configuration) (_ : identifier) (_ : simple_expression * value) (_ : statement list)
        : unit =
      ()

    method! record_fun_def
        (_ : configuration) (_ : identifier) (_ : identifier list) (_ : statement list) : unit =
      ()

    method! record_expr_stmt (_ : configuration) (_ : expression * value) : unit = ()

    (* Initializing the program, so create a stack frame for main$. *)
    method! program_entry ({ cur_fun = fun_id; _ } : configuration) : unit =
      self#debug_print @@ "PROGRAM ENTRY" ;
      self#push_stack @@ make_astack_frame ~fun_id ()

    (* Exiting the program, so pop main$'s stack frame and assert that the stack is empty. *)
    method! program_exit (_ : configuration) : unit =
      self#debug_print @@ "PROGRAM EXIT" ;
      let _ = self#pop_stack in
      assert (List.is_empty astack_)

    method! dump_table : unit = ()
  end

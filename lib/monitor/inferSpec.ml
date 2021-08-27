open Containers
open CCFun.Infix
open Expr
open Common

let debug = true

module Lattice = struct
  (* For now, we model vectors as a single value, with everything merged.
     Not_NA is perhaps better interpreted as "does not contain NAs."

          Top
         /   \
       NA   Not_NA
         \   /
          Bot
  *)
  type t =
    | Top [@printer fun fmt _ -> fprintf fmt "⊤"]
    | NA [@printer fun fmt _ -> fprintf fmt "NA"]
    | Not_NA [@printer fun fmt _ -> fprintf fmt "¬NA"]
    | Bot [@printer fun fmt _ -> fprintf fmt "⊥"]
  [@@deriving eq, show]

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

  (* alpha is the abstraction function, mapping a concrete value to a lattice value *)
  let alpha x =
    let data = vector_data x in
    let res = Array.exists (fun x -> is_na x) data in
    match res with
    | true -> NA
    | false -> Not_NA

  (* this version of alpha abstracts literals *)
  let alpha_lit x =
    match is_na x with
    | true -> NA
    | false -> Not_NA
end
type lattice = Lattice.t [@@deriving eq, show]

(* need some indirection: we store avalues in a pool because they are mutable *)
type avalue_id = int [@@deriving show]
type aenvironment = avalue_id Env.t

(* set of avalue ids *)
module AValueSet = Set.Make (struct
  type t = avalue_id
  let compare = Ord.int
end)
type avalue_set = AValueSet.t [@@deriving eq]

(* avalue, an abstract value
    - lower: lower bound on the lattice for the avalue
    - upper: upper bound on the lattice for the avalue
    - deps: ids of the avalue that this avalue depends on
*)
type avalue =
  { lower : lattice [@default Lattice.Bot]
  ; upper : lattice [@default Lattice.Top]
  ; deps : avalue_set [@default AValueSet.empty]
  }
[@@deriving eq, make]

let show_avalue { lower; upper; deps } =
  let deps_str =
    if AValueSet.is_empty deps then ""
    else AValueSet.to_string ~start:"; {" ~stop:"}" show_avalue_id deps in
  Printf.sprintf "[%s, %s%s]" (show_lattice lower) (show_lattice upper) deps_str

(* abstract stack frame *)
type astack_frame =
  { fun_id : identifier
  ; aenv : aenvironment [@default Env.empty]
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
      if debug then (
        let n = List.length astack_ - 1 in
        if n > 0 then Stdlib.print_string (String.make (n * 2) ' ') ;
        Stdlib.print_endline str)

    (* Push a frame onto the shadow stack *)
    method private push_stack (frame : astack_frame) : unit = astack_ <- frame :: astack_

    (* Pop a frame from the shadow stack, and return the frame. *)
    method private pop_stack : astack_frame =
      assert (List.length astack_ > 0) ;
      let top, rest = List.hd_tl astack_ in
      astack_ <- rest ;
      top

    (* Read the top frame but don't pop the stack. *)
    method private top_frame : astack_frame =
      assert (List.length astack_ > 0) ;
      List.hd astack_

    (* Replace the top stack frame with the given frame. *)
    method private update_top_frame (frame : astack_frame) : unit =
      assert (List.length astack_ > 0) ;
      ignore self#pop_stack ;
      self#push_stack frame

    (* Add an avalue to the pool, and return its id. *)
    method private push_avalue (aval : avalue) : avalue_id * avalue =
      let id = Vector.length aval_pool_ in
      Vector.push aval_pool_ aval ;
      assert (equal_avalue (Vector.get aval_pool_ id) aval) ;
      (id, aval)

    (* Given an id, return its avalue from the pool. *)
    method private get_avalue (id : avalue_id) : avalue = Vector.get aval_pool_ id

    (* Given an id and a new avalue, replace the old avalue in the pool. *)
    (* TODO: maybe later we can specialize this, e.g. set_lower, set_upper, set_deps *)
    method private set_avalue (id : avalue_id) (new_aval : avalue) : unit =
      Vector.set aval_pool_ id new_aval

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
      self#debug_print @@ "--> Entering " ^ fun_id ;
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
      assert (equal_identifier popped_fun fun_id) ;
      self#debug_print @@ "<-- Exiting " ^ popped_fun

    method! record_assign
        ({ cur_fun; _ } : configuration) (x : identifier) ((e, v) : expression * value) : unit =
      let collect_lits = List.rev % ExprFold.literals#expr [] in
      let collect_vars = List.rev % ExprFold.variables#expr [] in

      let ({ fun_id; aenv } as frame) = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print @@ Printf.sprintf "%s <- %s" x (Deparser.expr_to_r e) ;

      match e with
      | Simple_Expression (Lit _) ->
          (* Create a new abstract value for the result, and add it to the avalue pool.
              Then update the abstract env so that x refers to the new aval. *)
          let aid, aval = self#push_avalue @@ make_avalue ~lower:(Lattice.alpha v) () in
          self#update_top_frame { frame with aenv = Env.add x aid aenv } ;
          self#debug_print
          @@ Printf.sprintf "  # aenv: %s ↦ %d | aval pool: %d ↦ %s" x aid aid (show_avalue aval)
      | Simple_Expression (Var y) ->
          (* Look up the id for y's abstract value.
             Then update the abstract env so x refers to y's aval. *)
          (* TODO: workaround for WIP where y isn't in the environment *)
          (* assert (Env.mem y aenv) ; *)
          Env.get y aenv
          |> Option.iter (fun yid ->
                 self#update_top_frame { frame with aenv = Env.add x yid aenv } ;
                 self#debug_print @@ Printf.sprintf "  # aenv: %s ↦ %d" x yid)
      | Combine _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _ ->
          let lits, vars = Pair.(collect_lits &&& collect_vars) e in
          (* Create new avals for each literal, look up the avals for each variable. *)
          let alits =
            List.map (fun l -> self#push_avalue @@ make_avalue ~lower:(Lattice.alpha_lit l) ()) lits
          in
          let avars = List.filter_map (fun y -> Env.get y aenv) vars in

          (* Create a new aval for the expression result, setting its dependencies and updating the
             abstract env. *)
          let deps = AValueSet.of_list @@ List.map Stdlib.fst alits @ avars in
          let aid, aval = self#push_avalue @@ make_avalue ~lower:(Lattice.alpha v) ~deps () in
          self#update_top_frame { frame with aenv = Env.add x aid aenv } ;

          (if not @@ List.is_empty alits then
           let lit_str =
             List.to_string ~sep:", "
               (fun (aid, aval) -> Printf.sprintf "%d ↦ %s" aid (show_avalue aval))
               alits in
           self#debug_print @@ Printf.sprintf "  # aval pool: %s" lit_str) ;
          self#debug_print
          @@ Printf.sprintf "  # aenv: %s ↦ %d | aval pool: %d ↦ %s" x aid aid (show_avalue aval)
      | Call _ ->
          (* TODO: Needs special handling; for now, create a placeholder aval for the result *)
          let aid, aval = self#push_avalue @@ make_avalue ~lower:(Lattice.alpha v) () in
          self#update_top_frame { frame with aenv = Env.add x aid aenv } ;
          self#debug_print
          @@ Printf.sprintf "  # aenv: %s ↦ %d | aval pool: %d ↦ %s" x aid aid (show_avalue aval)
      | Dataframe_Ctor _ -> raise Not_supported

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
        ({ cur_fun; _ } : configuration)
        (x1 : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      let { fun_id; aenv } = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print
      @@ Printf.sprintf "%s[[%s]] <- %s" x1 (Deparser.simple_expr_to_r se2)
           (Deparser.simple_expr_to_r se3) ;

      (* Weak update for x1: its abstract val has new depedencies added. *)
      assert (Env.mem x1 aenv) ;
      let aid = Env.find x1 aenv in
      let aval = self#get_avalue aid in

      (* TODO: workaround for WIP where y isn't in the environment *)
      (match se3 with
      | Lit l ->
          (* Create a new abstract val, that x1 now depends on. *)
          let aid, aval = self#push_avalue @@ make_avalue ~lower:(Lattice.alpha_lit l) () in
          self#debug_print @@ Printf.sprintf "  # aval pool: %d ↦ %s" aid (show_avalue aval) ;
          Some aid
      | Var y ->
          (* Look up y's abstract val, that x1 now depends on. *)
          Env.get y aenv)
      |> Option.iter (fun id ->
             let aval' = { aval with deps = AValueSet.add id aval.deps } in
             self#set_avalue aid aval' ;
             self#debug_print
             @@ Printf.sprintf "  # aenv: %s ↦ %d | aval pool: %d ↦ %s" x1 aid aid
                  (show_avalue aval'))

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

    (* Exiting the program, so pop main$'s stack frame *)
    method! program_exit (_ : configuration) : unit =
      self#debug_print @@ "PROGRAM EXIT" ;
      (* We might have an abnormal exit, so clean up the stack. *)
      astack_ <- []

    method! dump_table : unit = ()
  end

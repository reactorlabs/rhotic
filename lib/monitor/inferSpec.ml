open Containers
open CCFun.Infix
open Expr
open Common

let debug = true

module Lattice = struct
  (* For now, we model vectors as a single value, with everything merged.
     NA is perhaps better interpreted as "contains NAs," while
     Not_NA is interpreted as "does not contain NAs,"

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

    (* a stack that represents the nesting of for loops,
       used to remember the current loop variable and the (abstract) value being iterated over *)
    val mutable for_loop_stack_ : (identifier * avalue_id) list = []

    (******************************************************************************
     * Monitor helpers
     ******************************************************************************)

    (* Debug printing, with indenting to reflect the stack depth. *)
    method private debug_print (str : string) : unit =
      if debug then (
        let n = List.length astack_ - 1 + List.length for_loop_stack_ in
        if n > 0 then Stdlib.print_string (String.make (n * 2) ' ') ;
        Stdlib.print_endline str)

    (* Read the top frame but don't pop the stack. *)
    method private top_frame : astack_frame =
      assert (List.length astack_ > 0) ;
      List.hd astack_

    (* Replace the top stack frame with the given frame. *)
    method private update_top_frame (frame : astack_frame) : unit =
      assert (List.length astack_ > 0) ;
      astack_ <- frame :: List.tl astack_

    (* Add a new binding to the current (i.e. top frame's) abstract environment. *)
    method private add_env_binding (x : identifier) (id : avalue_id) : unit =
      let ({ aenv; _ } as frame) = self#top_frame in
      self#update_top_frame { frame with aenv = Env.add x id aenv } ;
      self#debug_print @@ Printf.sprintf "  # aenv: %s ↦ %d" x id

    (* Add an avalue to the pool, and return its id. *)
    method private push_avalue (aval : avalue) : avalue_id =
      let id = Vector.length aval_pool_ in
      Vector.push aval_pool_ aval ;
      assert (equal_avalue (Vector.get aval_pool_ id) aval) ;
      self#debug_print @@ Printf.sprintf "  # aval pool: %d ↦ %s" id (show_avalue aval) ;
      id

    (* Given an id, return its avalue from the pool. *)
    method private get_avalue (id : avalue_id) : avalue = Vector.get aval_pool_ id

    (* Given an id and a new avalue, replace the old avalue in the pool. *)
    method private set_avalue (id : avalue_id) (aval' : avalue) : unit =
      Vector.set aval_pool_ id aval' ;
      self#debug_print @@ Printf.sprintf "  # aval pool: %d ↦ %s" id (show_avalue aval')

    (******************************************************************************
     * Callbacks to record interpreter operations
     ******************************************************************************)

    (* TODO: add constraints *)
    (* In a sequence expression x:y, x and y must not be NA. *)
    method! record_binary_op
        (_ : configuration)
        (_ : binary_op)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* TODO: add constraints *)
    (* In a subset2 expression x[[i]], i must not be NA. *)
    method! record_subset2
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : simple_expression * value)
        (_ : value) : unit =
      ()

    (* Initialize a stack frame and push onto the shadow stack. *)
    method! record_call_entry
        (_ : configuration) (fun_id : identifier) (_ : simple_expression list * value list) : unit =
      let frame = make_astack_frame ~fun_id () in
      self#debug_print @@ "--> Entering " ^ frame.fun_id ;
      astack_ <- frame :: astack_

    (* Exiting a function call, so pop the shadow stack.
       Assert that the call we're exiting corresponds to the popped stack frame. *)
    method! record_call_exit
        (_ : configuration)
        (fun_id : identifier)
        (_ : simple_expression list * value list)
        (_ : value) : unit =
      assert (List.length astack_ > 0) ;
      let top, rest = List.hd_tl astack_ in
      assert (equal_identifier top.fun_id fun_id) ;
      astack_ <- rest ;
      self#debug_print @@ "<-- Exiting " ^ top.fun_id

    method! record_assign
        ({ cur_fun; _ } : configuration) (x : identifier) ((e, v) : expression * value) : unit =
      let collect_lits = List.rev % ExprFold.literals#expr [] in
      let collect_vars = List.rev % ExprFold.variables#expr [] in

      let { fun_id; aenv } = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print @@ Deparser.stmt_to_r @@ Assign (x, e) ;

      match e with
      | Simple_Expression (Lit l) ->
          (* Create a new abstract value for the result, and add it to the avalue pool.
              Then update the abstract env so that x refers to the new aval. *)
          make_avalue ~lower:(Lattice.alpha_lit l) () |> self#push_avalue |> self#add_env_binding x
      | Simple_Expression (Var y) ->
          (* Look up the id for y's abstract value.
             Then update the abstract env so x refers to y's aval. *)
          (* TODO: workaround for WIP where y isn't in the environment *)
          (* assert (Env.mem y aenv) ; *)
          Env.get y aenv |> Option.iter (fun yid -> self#add_env_binding x yid)
      | Combine _ | Unary_Op _ | Binary_Op _ | Subset1 _ | Subset2 _ ->
          (* Collect deps: create new avals for each literal, look up avals for each variable. *)
          let alits =
            collect_lits e
            |> List.map (fun l -> self#push_avalue @@ make_avalue ~lower:(Lattice.alpha_lit l) ())
          in
          (* TODO: workaround for WIP where y isn't in the environment *)
          let avars = collect_vars e |> List.filter_map (fun y -> Env.get y aenv) in

          (* Create a new aval (with deps) for the expression result, and update the aenv *)
          make_avalue ~lower:(Lattice.alpha v) ~deps:(AValueSet.of_list (alits @ avars)) ()
          |> self#push_avalue |> self#add_env_binding x
      | Call _ ->
          (* TODO: Needs special handling; for now, create a placeholder aval for the result *)
          make_avalue ~lower:(Lattice.alpha v) () |> self#push_avalue |> self#add_env_binding x
      | Dataframe_Ctor _ -> raise Not_supported

    (* TODO: add constraints *)
    (* In a subset1 assignment x[i] <- v, i must not be NA. *)
    method! record_subset1_assign
        ({ cur_fun; env; _ } : configuration)
        (x1 : identifier)
        ((opt_se2, _) : simple_expression option * value option)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      let { fun_id; aenv } = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print @@ Deparser.stmt_to_r @@ Subset1_Assign (x1, opt_se2, se3) ;

      (* Update for x1: its aval might have new deps and a new lower bound. *)
      assert (Env.mem x1 aenv) ;
      let aid = Env.find x1 aenv in
      let aval = self#get_avalue aid in

      (* TODO: workaround for WIP where y isn't in the environment *)
      (* If se3 is a lit, create a new aval that x1's aval depends on.
         If se3 is a var y, then x1's aval depends on y's aval. *)
      (match se3 with
      | Lit l -> make_avalue ~lower:(Lattice.alpha_lit l) () |> self#push_avalue |> Option.some
      | Var y -> Env.get y aenv)
      |> Option.iter (fun id ->
             (* Update the aval's lower bound, based on the concrete val. *)
             let lower = Lattice.alpha (Env.find x1 env) in
             (* If opt_se2 is Some se2, then it's a weak update.
                If opt_se2 is None, then it's a strong update, since we're overwriting the vector. *)
             let deps =
               match opt_se2 with
               | Some _ -> AValueSet.add id aval.deps
               | None -> AValueSet.singleton id in
             self#set_avalue aid { aval with lower; deps })

    (* TODO: add constraints *)
    (* In a subset2 assignment x[[i]] <- v, i must not be NA. *)
    method! record_subset2_assign
        ({ cur_fun; env; _ } : configuration)
        (x1 : identifier)
        ((se2, _) : simple_expression * value)
        ((se3, _) : simple_expression * value)
        (_ : value) : unit =
      let { fun_id; aenv } = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print @@ Deparser.stmt_to_r @@ Subset2_Assign (x1, se2, se3) ;

      (* Weak update for x1: its abstract val has new depedencies added. *)
      assert (Env.mem x1 aenv) ;
      let aid = Env.find x1 aenv in
      let aval = self#get_avalue aid in

      (* TODO: workaround for WIP where y isn't in the environment *)
      (* If se3 is a lit, create a new aval that x1's aval depends on.
         If se3 is a var y, then x1's aval depends on y's aval. *)
      (match se3 with
      | Lit l -> make_avalue ~lower:(Lattice.alpha_lit l) () |> self#push_avalue |> Option.some
      | Var y -> Env.get y aenv)
      |> Option.iter (fun id ->
             let lower = Lattice.alpha (Env.find x1 env) in
             let deps = AValueSet.add id aval.deps in
             self#set_avalue aid { aval with lower; deps })

    (* In an if statement, the condition cannot be NA. *)
    method! record_if_entry
        (_ : configuration)
        (_ : simple_expression * value)
        (_ : statement list)
        (_ : statement list) : unit =
      ()

    (* Get the aval for the vector we're looping over, and add it to the for loop stack. *)
    method! record_for_entry
        ({ cur_fun; _ } : configuration)
        (x : identifier)
        ((se, _) : simple_expression * value)
        (_ : statement list) : unit =
      let { fun_id; aenv } = self#top_frame in
      assert (equal_identifier cur_fun fun_id) ;
      self#debug_print @@ Printf.sprintf "for (%s in %s) {" x (show_simple_expression se) ;

      (* If the vector is a lit, create an aval. If it's a var, look up its aval. *)
      let aid =
        match se with
        | Lit l -> make_avalue ~lower:(Lattice.alpha_lit l) () |> self#push_avalue
        | Var y -> Env.find y aenv in
      for_loop_stack_ <- (x, aid) :: for_loop_stack_

    (* Iterating through a vector, so create aval for the current vector element. *)
    method! record_for_iteration
        (_ : configuration) ((x, v) : identifier * value) (_ : value) (_ : statement list) : unit =
      assert (List.length for_loop_stack_ > 0) ;
      let x', seq_id = List.hd for_loop_stack_ in
      assert (equal_identifier x x') ;

      self#debug_print @@ Printf.sprintf "  # iterating 'for (%s in ...)'" x ;
      make_avalue ~lower:(Lattice.alpha v) ~deps:(AValueSet.singleton seq_id) ()
      |> self#push_avalue |> self#add_env_binding x

    method! record_for_exit (_ : configuration) (_ : value) : unit =
      assert (List.length for_loop_stack_ > 0) ;
      let (x, _), rest = List.hd_tl for_loop_stack_ in
      for_loop_stack_ <- rest ;
      self#debug_print @@ Printf.sprintf "} # exiting 'for (%s in ...)'" x

    method! record_fun_def
        (_ : configuration) (_ : identifier) (_ : identifier list) (_ : statement list) : unit =
      ()

    method! record_print_stmt (_ : configuration) ((e, _) : expression * value) : unit =
      self#debug_print @@ Deparser.stmt_to_r (Print e)

    method! record_expr_stmt (_ : configuration) (_ : expression * value) : unit = ()

    (* Initializing the program, so create a stack frame for main$. *)
    method! program_entry ({ cur_fun = fun_id; _ } : configuration) : unit =
      self#debug_print @@ "=== PROGRAM ENTRY ===" ;
      (* Initializing the stack with a frame for the top level. *)
      astack_ <- [ make_astack_frame ~fun_id () ]

    (* Exiting the program, so pop main$'s stack frame *)
    method! program_exit (_ : configuration) : unit =
      self#debug_print @@ "=== PROGRAM EXIT ===" ;
      (* We might have an abnormal exit, so clean up the stack. *)
      astack_ <- []

    method! dump_table : unit = ()
  end

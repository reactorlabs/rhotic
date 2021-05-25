open Expr
open Util

(*
              Top
             /   \
            /     \
         Num      Str
       /     \       |
    Bool     Int    /
        \     |    /
          \   |  /
             Bot
*)

type abstract_type =
  | Top
  | Str
  | Num
  | Int
  | Bool
  | Bot
[@@deriving eq, show { with_path = false }]

let join_type a b =
  match (a, b) with
  (* bot and top cases *)
  | Bot, x | x, Bot -> x
  | Top, _ | _, Top -> Top
  (* a = b cases*)
  | Bool, Bool -> Bool
  | Int, Int -> Int
  | Str, Str -> Str
  | Num, Num -> Num
  (* harder cases *)
  | Bool, Int | Int, Bool -> Num
  | Num, (Bool | Int) | (Bool | Int), Num -> Num
  | Str, _ | _, Str -> Top

let to_abstract_type = function
  | T_Bool -> Bool
  | T_Int -> Int
  | T_Str -> Str

(*
- Record concrete types
  - map of function ID to list of list of concrete types
  - map of function ID to list of sets of concrete types
- Record abstract types
  ---> map of function ID to list of abstract types
*)
type abstract_function_types = abstract_type list FunTab.t

class monitor =
  object
    val mutable recorded_functions = FunTab.empty

    (* Update the entry for the called function.
       If this method is called, then the function must exist. *)
    method record_call (_ : configuration) (_ : value) (id : identifier) (args : value list) : unit
        =
      let recorded_types = FunTab.find id recorded_functions in
      let abstract_arg_types = List.map (vector_type %> to_abstract_type) args in
      let new_types = List.map2 join_type recorded_types abstract_arg_types in
      recorded_functions <- FunTab.add id new_types recorded_functions

    (* Create an empty entry, with the args list initialized to Bot *)
    method record_fun_def (_ : configuration) (id : identifier) (params : identifier list) : unit =
      let n = List.length params in
      let init = List.init n (fun _ -> Bot) in
      recorded_functions <- FunTab.add id init recorded_functions

    method dump_table =
      let bindings = FunTab.to_seq recorded_functions in
      let f (id, args) =
        let output = List.map show_abstract_type args |> String.concat ", " in
        Printf.printf "\tfunction %s: %s\n" id output in
      Stdlib.print_endline ">>> Dumping table of function types <<<" ;
      Seq.iter f bindings ;
      Stdlib.print_endline ">>> Finished <<<"
  end

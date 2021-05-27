open Expr
open Common
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

(* Map of function ID to abstract signature
   The abstract signature is a list of abstract types, where the FIRST element is the return type
   and the rest of the list represents the parameter types, in order *)
type abstract_function_types = abstract_type list FunTab.t

class monitor =
  object
    val mutable recorded_functions : abstract_function_types = FunTab.empty

    (* Update the entry for the called function.
       If this method is called, then the function must exist. *)
    method record_call (_ : configuration) (ret : value) (id : identifier) (args : value list)
        : unit =
      let recorded_types = FunTab.find id recorded_functions in
      let abstract_arg_types =
        (to_abstract_type @@ vector_type ret) :: List.map (vector_type %> to_abstract_type) args
      in
      let new_types = List.map2 join_type recorded_types abstract_arg_types in
      recorded_functions <- FunTab.add id new_types recorded_functions

    (* Create an empty entry, with the args list initialized to Bot *)
    method record_fun_def (_ : configuration) (id : identifier) (params : identifier list) : unit =
      (* Add 1 for the return type *)
      let n = List.length params + 1 in
      let init = List.init n (fun _ -> Bot) in
      recorded_functions <- FunTab.add id init recorded_functions

    method dump_table () : unit =
      Stdlib.print_endline ">>> FunctionTypesElementwiseMerge: dumping table <<<" ;
      let f (id, types) =
        let type_str = List.map show_abstract_type types in
        let ret, args = (List.hd type_str, List.tl type_str) in
        Printf.printf "\t%s: (%s) -> %s\n" id (String.concat ", " args) ret in
      FunTab.to_seq recorded_functions |> Seq.iter f
  end

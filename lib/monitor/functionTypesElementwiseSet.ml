open Expr
open Common

module TypeSet = Set.Make (struct
  type t = type_tag
  let compare = compare_type_tag
end)

(* Map of function ID to abstract signature
   The abstract signature is a list of sets of concrete types, where the FIRST element is the return
   type and the rest of the list represents the parameter types, in order *)
type abstract_function_types = TypeSet.t list FunTab.t

class monitor =
  object
    val mutable recorded_functions = FunTab.empty

    (* Update the entry for the called function.
       If this method is called, then the function must exist. *)
    method record_call (_ : configuration) (ret : value) (id : identifier) (args : value list)
        : unit =
      let recorded_types = FunTab.find id recorded_functions in
      let observed_types = vector_type ret :: List.map vector_type args in
      let new_types = List.map2 (fun set t -> TypeSet.add t set) recorded_types observed_types in
      recorded_functions <- FunTab.add id new_types recorded_functions

    (* Create an empty entry, with the args list initialized to Bot *)
    method record_fun_def (_ : configuration) (id : identifier) (params : identifier list) : unit =
      (* Add 1 for the return type *)
      let n = List.length params + 1 in
      let init = List.init n (fun _ -> TypeSet.empty) in
      recorded_functions <- FunTab.add id init recorded_functions

    method dump_table () : unit =
      Stdlib.print_endline ">>> FunctionTypesElementwiseSet: dumping table <<<" ;
      let f (id, types) =
        let set_to_s ts =
          let inner = TypeSet.elements ts |> List.map show_type_tag |> String.concat "," in
          "{" ^ inner ^ "}" in
        let type_str = List.map set_to_s types in
        let ret, args = (List.hd type_str, List.tl type_str) in
        Printf.printf "\t%s: (%s) -> %s\n" id (String.concat ", " args) ret in
      FunTab.to_seq recorded_functions |> Seq.iter f
  end

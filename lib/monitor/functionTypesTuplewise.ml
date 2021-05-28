open Expr
open Common

module SignatureSet = Set.Make (struct
  type t = type_tag list
  let compare = List.compare compare_type_tag
end)

(* Map of function ID to abstract signature
   The abstract signature is a set of concrete signatures, where the FIRST element is the return
   type and the rest of the list represents the parameter types, in order *)
type function_types = SignatureSet.t FunTab.t

class monitor =
  object
    inherit Monitor.monitor

    val mutable recorded_functions : function_types = FunTab.empty

    (* Update the entry for the called function.
       If this method is called, then the function must exist. *)
    method! record_call (_ : configuration) (ret : value) (id : identifier) (args : value list)
        : unit =
      let recorded_types = FunTab.find id recorded_functions in
      let observed_types = vector_type ret :: List.map vector_type args in
      let new_types = SignatureSet.add observed_types recorded_types in
      recorded_functions <- FunTab.add id new_types recorded_functions

    (* Create an empty entry, with the args list initialized to Bot *)
    method! record_fun_def (_ : configuration) (id : identifier) (_ : identifier list) : unit =
      recorded_functions <- FunTab.add id SignatureSet.empty recorded_functions

    method! dump_table : unit =
      Stdlib.print_endline ">>> FunctionTypesTuplewise: dumping table <<<" ;
      let f (id, sig_set) =
        Printf.printf "\t%s\n" id ;
        let g types =
          let type_str = List.map show_type_tag types in
          let ret, args = (List.hd type_str, List.tl type_str) in
          Printf.printf "\t\t(%s) -> %s\n" (String.concat ", " args) ret in
        SignatureSet.iter g sig_set in
      FunTab.to_seq recorded_functions |> Seq.iter f
  end

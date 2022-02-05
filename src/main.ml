open Containers
open Lib

let init_state = Eval2.make_state ~program:(Vector.of_list []) ~pc:0 ()

(* TODO
   - repl mode
     - need to carry program over
     - debug printing opcodes, start printing from offset
     - test repl with functions
     - handle repl printing result with Print builtin... looks like it works
   - cleanup main
   - remove old eval and monitors

   - debugging: print execution trace
   - better interface for state
   - cleanup and reorganize eval/expr/common

   - implement eval instruction

   - abstract interpreter
     - need some abstract state / analysis
   - dynamic interpreter
*)

let parse_to_r input =
  try Parser.parse input |> Deparser.to_r |> Stdlib.print_endline
  with Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg

let run ?(debug = false) input =
  try
    let code = Parser.parse input in
    let program, pc = Compile.compile code in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      Printf.eprintf "%s" @@ Vector.to_string ~sep:"\n" (Opcode.show_pc_opcode pc) program ;
      Printf.eprintf "; start pc = %d\n\n" pc ;
      Printf.eprintf "Execution trace:\n%!") ;

    let state' = Eval2.(eval_continuous ~debug @@ make_state ~program ~pc ()) in
    match state'.last_val with
    | None -> ()
    | Some v -> Stdlib.print_endline @@ Expr.show_val v
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.printf "Error: %s\n" @@ Common.excptn_to_string e) ;
    exit 255

let () =
  let usage_msg = Printf.sprintf "rhotic [-f <file> [--to-r]]" in

  let path = ref "" in
  let debug = ref false in
  let to_r = ref false in

  let cmd_args =
    [ ( "-f"
      , Arg.String
          (fun s ->
            if Sys.file_exists s then path := s else raise @@ Arg.Bad ("No such file: " ^ s))
      , "rhotic file to run" )
    ; ("-d", Arg.Set debug, "Print debugging info")
    ; ("--debug", Arg.Set debug, "Print debugging info")
    ; ("--to-r", Arg.Set to_r, "Translate rhotic code to R")
    ] in

  Arg.parse cmd_args (fun s -> raise @@ Arg.Bad ("Invalid argument " ^ s)) usage_msg ;

  if String.equal !path "" then Stdlib.print_endline usage_msg
  else
    let chan = Stdlib.open_in !path in
    let input = Stdlib.really_input_string chan @@ Stdlib.in_channel_length chan in
    Stdlib.close_in chan ;

    if !to_r then parse_to_r input else run ~debug:!debug input

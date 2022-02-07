open Containers
open Lib
open Util

(* TODO
   - remove old eval and monitors

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

let run ?(debug = false) ?(exit_on_error = false) ?(state = Eval2.init_state) input =
  try
    let old_program = state.program in
    let code = Parser.parse input in
    let program, pc = Compile.compile ~program:old_program code in
    let state' = { state with program; pc; last_val = None } in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      (* Skip the program before the current pc, i.e. the fragment we already printed. *)
      program |> Vector.to_array
      |> Array.filter_mapi (fun i op ->
             if i < state.pc then None else Some (Opcode.show_pc_opcode i op))
      |> Array.to_string ~sep:"\n" Fun.id |> Printf.eprintf "%s" ;
      Printf.eprintf "; start pc = %d\n\n" pc ;
      Printf.eprintf "Execution trace:\n%!") ;

    let state' = Eval2.(eval_continuous ~debug state') in
    (match state'.last_val with
    | None -> ()
    | Some v -> Stdlib.print_endline @@ Expr.show_val v) ;
    state'
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.printf "Error: %s\n" @@ Common.excptn_to_string e) ;
    if exit_on_error then exit 255 else state

let repl ?(debug = false) () =
  let repl_help () =
    Stdlib.print_endline "Enter a rhotic expression to be evaluated." ;
    Stdlib.print_endline "Type '#h' to print this message." ;
    Stdlib.print_endline "Type '#r' to reset the REPL." ;
    Stdlib.print_endline "Type '#to_r [code]' to translate 'code' to R syntax." ;
    Stdlib.print_endline "Type '#q' or CTRL+D to quit." in

  let handle_directive state input =
    if String.equal input "#h" then (
      repl_help () ;
      state)
    else if String.equal input "#r" then Eval2.init_state
    else if String.equal input "#q" then raise End_of_file
    else
      match String.chop_prefix ~pre:"#to_r" input with
      | Some str ->
          parse_to_r str ;
          state
      | None ->
          Printf.printf "Error: unknown directive\n" ;
          state in

  let rec loop state =
    Stdlib.print_string "> " ;
    let input = Stdlib.read_line () in
    let state' =
      if String.prefix ~pre:"# " input then state
      else if String.prefix ~pre:"#" input then handle_directive state input
      else if String.(trim input = "") then state
      else run ~debug ~state input in
    (loop [@tailcall]) state' in

  Stdlib.print_endline "Welcome to the rhotic REPL.\n" ;
  repl_help () ;
  try loop Eval2.init_state with End_of_file -> Stdlib.print_endline "\nGoodbye!"

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

  if String.equal !path "" then repl ~debug:!debug ()
  else
    let chan = Stdlib.open_in !path in
    let input = Stdlib.really_input_string chan @@ Stdlib.in_channel_length chan in
    Stdlib.close_in chan ;

    if !to_r then parse_to_r input else Stdlib.ignore @@ run ~debug:!debug ~exit_on_error:true input

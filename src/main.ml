open Containers
open Lib
open Util

(* TODO
   - dynamic interpreter
   - implement eval instruction
*)

module TypeAnalysis = Analysis.Make (TypeAnalysisImpl)

(* Duplicate code for now; it doesn't make sense to run analysis in the repl *)
let run_once ?(debug = false) ?(analysis = false) ?(run = true) input =
  try
    let code = Parser.parse input in
    let program, pc = Compile.compile code in
    let state = Eval.State.make ~program ~pc () in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      program
      |> Vector.mapi (fun i op -> Opcode.show_pc_opcode i op)
      |> Vector.to_string ~sep:"\n" Fun.id |> Printf.eprintf "%s" ;
      Printf.eprintf "; start pc = %d\n\n" pc) ;

    if run then (
      Printf.eprintf "Execution trace:\n%!" ;
      let state' = Eval.eval_continuous ~debug state in
      match Eval.State.last_val state' with
      | None -> ()
      | Some v -> Stdlib.print_endline @@ Expr.show_val v) ;

    if analysis then (
      Printf.eprintf "Analysis trace:\n%!" ;
      ignore @@ TypeAnalysis.analyze ~debug program pc)
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.printf "Error: %s\n" @@ Common.excptn_to_string e) ;
    exit 255

let parse_to_r input =
  try Parser.parse input |> Deparser.to_r |> Stdlib.print_endline
  with Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg

let run ?(debug = false) ?(state = Eval.State.init) input =
  try
    let code = Parser.parse input in
    let old_program, old_pc = Eval.State.program_pc state in
    let program, pc = Compile.compile ~program:old_program code in
    let state' = Eval.State.set_program_pc (program, pc) state in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      (* Skip the program before the current pc, i.e. the fragment we already printed. *)
      program |> Vector.to_array
      |> Array.filter_mapi (fun i op ->
             if i < old_pc then None else Some (Opcode.show_pc_opcode i op))
      |> Array.to_string ~sep:"\n" Fun.id |> Printf.eprintf "%s" ;
      Printf.eprintf "; start pc = %d\n\n" pc ;
      Printf.eprintf "Execution trace:\n%!") ;

    let state' = Eval.eval_continuous ~debug state' in
    (match Eval.State.last_val state' with
    | None -> ()
    | Some v -> Stdlib.print_endline @@ Expr.show_val v) ;
    state'
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.printf "Error: %s\n" @@ Common.excptn_to_string e) ;
    state

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
    else if String.equal input "#r" then Eval.State.init
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
  try loop Eval.State.init with End_of_file -> Stdlib.print_endline "\nGoodbye!"

let () =
  let usage_msg = Printf.sprintf "rhotic [-f <file> [--to-r]]" in

  let path = ref "" in
  let debug = ref false in
  let analysis = ref false in
  let run = ref true in
  let to_r = ref false in

  let cmd_args =
    [ ( "-f"
      , Arg.String
          (fun s ->
            if Sys.file_exists s then path := s else raise @@ Arg.Bad ("No such file: " ^ s))
      , "rhotic file to run" )
    ; ("-d", Arg.Set debug, "Print debugging info")
    ; ("--debug", Arg.Set debug, "Print debugging info")
    ; ("-a", Arg.Set analysis, "Run static analysis")
    ; ("--analysis", Arg.Set analysis, "Run static analysis")
    ; ("--no-run", Arg.Clear run, "Skip concrete execution")
    ; ("--to-r", Arg.Set to_r, "Translate rhotic code to R")
    ] in

  Arg.parse cmd_args (fun s -> raise @@ Arg.Bad ("Invalid argument " ^ s)) usage_msg ;

  if String.equal !path "" then repl ~debug:!debug ()
  else
    let chan = Stdlib.open_in !path in
    let input = Stdlib.really_input_string chan @@ Stdlib.in_channel_length chan in
    Stdlib.close_in chan ;

    if !to_r then parse_to_r input
    else Stdlib.ignore @@ run_once ~debug:!debug ~analysis:!analysis ~run:!run input

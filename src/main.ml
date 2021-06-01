open Lib
open Util

let parse_and_run monitors ?(conf = Eval.start) input =
  try
    let stmts = Parse.parse input in
    let conf', result = Eval.run_statements monitors conf stmts in
    Stdlib.print_endline @@ Expr.show_val result ;
    (* return the new configuration *)
    conf'
  with e ->
    (match e with
    | Parse.Parse_error msg -> Printf.printf "Parse error%s\n" msg
    | e -> Printf.printf "Error: %s\n" @@ Common.excptn_to_string e) ;
    (* return the old configuration *)
    conf

let parse_to_r input =
  try
    let stmts = Parse.parse input in
    Stdlib.print_endline @@ Deparse.to_r stmts
  with Parse.Parse_error msg -> Printf.printf "Parse error%s\n" msg

let repl monitors =
  let repl_help () =
    Stdlib.print_endline "Enter a rhotic expression to be evaluated." ;
    Stdlib.print_endline "Type '#h' to print this message." ;
    Stdlib.print_endline "Type '#m' to dump the monitor state." ;
    Stdlib.print_endline "Type '#r [code]' to translate 'code' to R syntax." ;
    Stdlib.print_endline "Type '#q' or CTRL+D to quit." in

  let run_once conf =
    let handle_directive input =
      if input = "#h" then repl_help ()
      else if input = "#m" then Monitor.dump_state monitors
      else if String.prefix ~pre:"#r " input then
        parse_to_r @@ Option.get @@ String.chop_prefix ~pre:"#r " input
      else if input = "#q" then raise End_of_file
      else Printf.printf "Error: unknown directive\n" in

    Stdlib.print_string "> " ;
    let input = Stdlib.read_line () in
    if String.prefix ~pre:"#" input then (
      handle_directive input ;
      conf)
    else if String.trim input = "" then conf
    else parse_and_run monitors ~conf input in

  let rec loop conf =
    let conf = run_once conf in
    (loop [@tailcall]) conf in

  Stdlib.print_endline "Welcome to the rhotic REPL.\n" ;
  repl_help () ;
  try loop Eval.start with End_of_file -> Stdlib.print_endline "\nGoodbye!"

let () =
  let monitors : Monitor.monitor list =
    [ new FunctionObservedNA.monitor
    ; new FunctionTypesTuplewise.monitor
    ; new FunctionTypesElementwiseSet.monitor
    ; new FunctionTypesElementwiseMerge.monitor
    ] in

  let usage_msg = Printf.sprintf "rhotic [-f <file> [--to-r]]" in

  let path = ref "" in
  let to_r = ref false in

  let cmd_args =
    [ ("-f", Arg.Set_string path, "rhotic file to run")
    ; ("--to-r", Arg.Set to_r, "Translate rhotic code to R")
    ] in

  Arg.parse cmd_args (fun s -> raise @@ Arg.Bad ("Invalid argument " ^ s)) usage_msg ;

  if !path = "" then repl monitors
  else
    let chan = Stdlib.open_in !path in
    let input = Stdlib.really_input_string chan @@ Stdlib.in_channel_length chan in
    Stdlib.close_in chan ;

    if !to_r then parse_to_r input else Stdlib.ignore @@ parse_and_run monitors input ;
    Monitor.dump_state monitors

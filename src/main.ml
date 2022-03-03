open Containers
open Lib
open Util

(* TODO: implement eval instruction
*)

module TypeAnalysis = Analysis.Make (TypeAnalysisImpl)

let run_once ?(debug = false) ?(analysis = false) ?(run = true) input =
  try
    let code = Parser.parse input in
    let program, pc = Compile.compile code in
    let state = EvalState.make ~program ~pc ~debug () in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      program
      |> Vector.mapi (fun i op -> Opcode.show_pc_opcode i op)
      |> Vector.to_string ~sep:"\n" Fun.id |> Printf.eprintf "%s" ;
      Printf.eprintf "; start pc = %d\n\n%!" pc) ;

    if run then (
      if debug then Printf.eprintf "Execution trace:\n%!" ;
      let state' = Eval.eval_continuous state in
      match EvalState.last_val state' with
      | None -> ()
      | Some v -> Stdlib.print_endline @@ Expr.show_val v) ;

    if analysis then (
      Printf.eprintf "Analysis trace:\n%!" ;
      ignore @@ TypeAnalysis.analyze ~debug program pc)
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.eprintf "Error: %s\n" @@ Common.excptn_to_string e) ;
    exit 255

let () =
  let usage_msg = Printf.sprintf "rhotic -f <file> [--to-r]" in

  let path = ref "" in
  let debug = ref false in
  let analysis = ref false in
  let run = ref true in
  let to_r = ref false in

  let cmd_args =
    [ ( "-f"
      , Arg.String
          (fun s -> if Sys.file_exists s then path := s else raise @@ Arg.Bad ("no such file " ^ s))
      , "rhotic file to run" )
    ; ("-d", Arg.Set debug, "Print debugging info")
    ; ("--debug", Arg.Set debug, "Print debugging info")
    ; ("-a", Arg.Set analysis, "Run static analysis")
    ; ("--analysis", Arg.Set analysis, "Run static analysis")
    ; ("--no-run", Arg.Clear run, "Skip concrete execution")
    ; ("--to-r", Arg.Set to_r, "Translate rhotic code to R")
    ] in

  Arg.parse cmd_args (fun s -> raise @@ Arg.Bad ("invalid argument " ^ s)) usage_msg ;

  if String.equal !path "" then (
    Printf.eprintf "%s: option '-f' is required.\n" Sys.argv.(0) ;
    Arg.usage cmd_args usage_msg ;
    exit 2)
  else
    let chan = Stdlib.open_in !path in
    let input = Stdlib.really_input_string chan @@ Stdlib.in_channel_length chan in
    Stdlib.close_in chan ;

    if !to_r then
      try Parser.parse input |> Deparser.to_r |> Stdlib.print_endline
      with Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    else Stdlib.ignore @@ run_once ~debug:!debug ~analysis:!analysis ~run:!run input

open Containers
open Lib
open Util

(* TODO: clean up analysis *)

let run_input debug static dynamic run input =
  try
    let program, pc = input |> Parser.parse |> Compile.compile in

    if debug then (
      Printf.eprintf "Compiled program:\n" ;
      Vector.iteri (fun i op -> Printf.eprintf "%s\n" @@ Opcode.show_pc_opcode i op) program ;
      Printf.eprintf "; start pc = %d\n%!" pc) ;

    if run then (
      if debug then Printf.eprintf "\nExecution trace:\n%!" ;
      match Eval.run ~debug (program, pc) with
      | Some v -> Printf.eprintf "%s\n%!" @@ Expr.show_val v
      | None -> ()) ;

    if static then (
      if debug then Printf.eprintf "\nStatic analysis trace:\n%!" ;
      ignore @@ Static.TypeAnalysis.analyze ~debug (program, pc)) ;

    if dynamic then (
      if debug then Printf.eprintf "\nDynamic analysis trace:\n%!" ;
      ignore @@ Dynamic.TypeAnalysis.run ~debug (program, pc))
  with e ->
    (match e with
    | Parser.Parse_error msg -> Printf.eprintf "Parse error%s\n" msg
    | e -> Printf.eprintf "Error: %s\n" @@ Common.excptn_to_string e) ;
    exit 255

let () =
  let usage_msg = Printf.sprintf "rhotic -f <file> [--to-r]" in

  let path = ref "" in
  let debug = ref false in
  let static = ref false in
  let dynamic = ref false in
  let run = ref true in
  let to_r = ref false in

  let cmd_args =
    [ ( "-f"
      , Arg.String
          (fun s -> if Sys.file_exists s then path := s else raise @@ Arg.Bad ("no such file " ^ s))
      , "rhotic file to run" )
    ; ("--debug", Arg.Set debug, "Print debugging info")
    ; ("-s", Arg.Set static, "Run static analysis")
    ; ("--static", Arg.Set static, "Run static analysis")
    ; ("-d", Arg.Set dynamic, "Run dynamic analysis")
    ; ("--dynamic", Arg.Set dynamic, "Run dynamic analysis")
    ; ("--no-run", Arg.Clear run, "Skip concrete execution")
    ; ("--to-r", Arg.Set to_r, "Compile rhotic code to R")
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
    else Stdlib.ignore @@ run_input !debug !static !dynamic !run input

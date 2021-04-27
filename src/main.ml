open Lib
open Util

let repl_help () =
  Stdlib.print_endline "Enter a rhotic expression to be evaluated." ;
  Stdlib.print_endline "Type '#h' to print this message." ;
  Stdlib.print_endline "Type '#r [code]' to translate 'code' to R syntax." ;
  Stdlib.print_endline "Type '#q' or CTRL+D to quit."

let run_once env =
  let handle_directive input =
    if input = "#h" then repl_help ()
    else if String.prefix ~pre:"#r " input then
      let r_input = Option.get @@ String.chop_prefix ~pre:"#r " input in
      try
        let stmts = Parse.parse r_input in
        Stdlib.print_endline @@ Deparse.to_r stmts
      with Parse.Parse_error msg -> Printf.printf "Parse error%s\n" msg
    else if input = "#q" then raise End_of_file
    else Printf.printf "Error: unknown directive\n" in

  let run input =
    try
      let stmts = Parse.parse input in
      let env', result = Eval.run_program env stmts in
      Stdlib.print_endline @@ Expr.show_val result ;
      (* return the new environment *)
      env'
    with e ->
      ( match e with
      | Parse.Parse_error msg -> Printf.printf "Parse error%s\n" msg
      | e ->
          let msg = Eval.excptn_to_string e in
          Printf.printf "Error: %s\n" msg ) ;
      (* return the old environment *)
      env in

  Stdlib.print_string "> " ;
  let input = Stdlib.read_line () in
  if String.prefix ~pre:"#" input then (
    handle_directive input ;
    env )
  else if String.trim input = "" then env
  else run input

let () =
  let rec loop env =
    let env' = run_once env in
    (loop [@tailcall]) env' in

  let env = Expr.Env.empty in
  Stdlib.print_endline "Welcome to the rhotic REPL.\n" ;
  repl_help () ;
  try loop env with End_of_file -> Stdlib.print_endline "\nGoodbye!"

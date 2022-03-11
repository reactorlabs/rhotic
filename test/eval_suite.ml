open Containers
open Lib
open Expr

(* When this executable is run with the environment variable `DUMP=file`, the test cases are
   dumped to `file` as an R script. Executing the R script ensures that evaluating the test cases
   as R code results in the same expected values. *)

module A = Alcotest

let pp_value ppf v = Fmt.pf ppf "%s" (show_val v)
let testable_value = A.testable pp_value equal_value

(* If we're dumping to an R script, open the file and output the test harness code. *)
let dump_file =
  match Sys.getenv_opt "DUMP" with
  | None -> None
  | Some file ->
      let fout = Stdlib.open_out file in
      let pf = Printf.fprintf fout in
      pf
        {|### Test harnesses
runtest <- function(desc, expected, code) {
  environment(code) <- parent.env(globalenv())
  stopifnot(identical( expected, code() ))
}

runerr <- function(desc, code) {
  environment(code) <- parent.env(globalenv())
  stopifnot(tryCatch(
    code(),
    finally = FALSE,
    error = function(c) TRUE,
    warning = function(c) TRUE
  ))
}

runok <- function(desc, code) {
  environment(code) <- parent.env(globalenv())
  tryCatch(
    invisible(code()),
    warning = function(c) stop("unexpected warning")
  )
}|} ;
      pf "\n\n" ;
      Some fout

(* Running the input failed unexpectedly, so construct a failing test and abort the dump. *)
let test_unexpected_fail desc e dump_file =
  let fail () = A.failf "Unexpected error: %s" (Printexc.to_string e) in
  if Option.is_some dump_file then fail () ;
  A.test_case desc `Quick fail

(* Create a test case that parses `input`, evaluates the code, and checks that it matches the
   `expected` value. *)
let test_eval desc (expected, input) =
  (* Also check that all elements of the vector have the right type. *)
  let assert_vec_elt_types = function
    | Vector _ as vec ->
        if not (Common.vector_consistent_type vec) then
          A.failf "Vector `%s` not consistent with its type!" (show_val vec)
    | Dataframe _ -> assert false in

  (* Dump the test case for R: evaluating `code` in R should return `expected` *)
  let dump code () =
    match dump_file with
    | Some fout ->
        Printf.fprintf fout "# %s\n" desc ;
        Printf.fprintf fout "runtest(\"%s\", %s, function(){%s})\n\n" desc
          (Deparser.val_to_r expected) (Deparser.to_r code)
    | _ -> () in

  (* Construct the actual test case *)
  let check res () =
    assert_vec_elt_types expected ;
    assert_vec_elt_types res ;
    A.(check testable_value) "same value" expected res in

  try
    let code = Parser.parse input in
    let res = code |> Compile.compile |> Eval.run |> Option.get_exn_or "expected value" in
    dump code () ;
    A.test_case desc `Quick (check res)
  with e -> test_unexpected_fail desc e dump_file

(* Create a test case that expects running `input` to throw an exception. *)
let test_eval_err desc ?(is_valid_r = false) (excptn, input) =
  (* Dump the test case for R: check that an expected error or warning is issued. *)
  let dump code () =
    match dump_file with
    | Some fout ->
        let comment, tester = if is_valid_r then ("OK", "runok") else ("ERROR", "runerr") in
        Printf.fprintf fout "# %s: %s\n" comment desc ;
        Printf.fprintf fout "%s(\"%s\", function(){%s})\n\n" tester desc (Deparser.to_r code)
    | _ -> () in

  (* Construct the actual test case *)
  let check code () =
    A.check_raises "same exception" excptn (fun _ -> ignore (Eval.run @@ Compile.compile code))
  in

  try
    let code = Parser.parse input in
    dump code () ;
    A.test_case desc `Quick (check code)
  with e -> test_unexpected_fail desc e dump_file

let () =
  let open Common in
  let open CCList.Infix in
  (* The compiler mangles variable names, so we can't manually construct exceptions and test for equality. *)
  let object_not_found ?(pre = "") id = Object_not_found (Identifier.prefix ~pre id) in

  A.run "eval-testsuite"
    (* The dummy test is so that we have something to run, when generating the R test suite. *)
    [ ("dummy", [ A.test_case "dummy test" `Quick (fun () -> A.(check int) "same value" 1 1) ])
    ; ( "literals"
      , [ test_eval "int 42" (vector_of int 42, "42")
        ; test_eval "int -1" (vector_of int ~-1, "-1")
        ; test_eval "int NA" (vector_of_na int, "NA_integer_")
        ; test_eval "bool true" (vector_of bool true, "T")
        ; test_eval "bool false" (vector_of bool false, "F")
        ; test_eval "bool NA" (vector_of_na bool, "NA")
        ; test_eval "string" (vector_of str "a", {|"a"|})
        ; test_eval "string empty" (vector_of str "", {|""|})
        ; test_eval "string NA" (vector_of_na str, "NA_character_")
        ] )
    ; ( "environments"
      , [ test_eval "lookup 1" (vector_of int 42, "x <- 42; x")
        ; test_eval "lookup 2" (vector_of int 1, "x <- 42; x <- 1; x")
        ; test_eval "lookup 3" (vector_of int 42, "x <- 42; y <- 1; z <- 2; x")
        ; test_eval "lookup 4" (vector_of int 2, "x <- 42; y <- 1; z <- 2; z")
        ; test_eval "lookup 5" (vector_of_list int (4 -- 1), "x <- 1; y <- 2; z <- 3; c(4, z, y, x)")
        ] )
    ; ("environments.err", [ test_eval_err "object not found" (object_not_found "x", "x") ])
    ; ( "combine"
        (* Workaround: we don't have the NULL vector, so we use an empty logical vector instead. *)
      , [ test_eval "empty" (null, "x <- c(); as.logical(x)")
        ; test_eval "single int" (vector_of int 42, "c(42)")
        ; test_eval "multiple int" (vector_of_list int (1 -- 3), "c(1, 2, 3)")
        ; test_eval "nested int"
            (vector_of_list int (5 -- 9), "x <- c(8); y <- c(6, 7, x, 9); c(5, y)")
        ; test_eval "nested int with empty int vector"
            (vector_of_list int [ 5; 6; 7; 9 ], "x <- 8[0]; y <- c(6, 7, x, 9); c(5, y)")
        ; test_eval "nested int with NA"
            ( vector_of_optlist int [ Some 5; None; Some 7; Some 8; Some 9 ]
            , "x <- 8; y <- c(NA_integer_, 7, x, 9); c(5, y)" )
        ; test_eval "single bool" (vector_of bool true, "c(T)")
        ; test_eval "multiple bool" (vector_of_list bool [ true; false; true ], "c(T, F, T)")
        ; test_eval "nested bool"
            ( vector_of_list bool [ true; true; false; false; true ]
            , "x <- c(F); y <- c(T, F, x, T); c(T, y)" )
        ; test_eval "nested bool with empty bool vector"
            ( vector_of_list bool [ true; true; false; true ]
            , "x <- F[0]; y <- c(T, F, x); c(T, y, T)" )
        ; test_eval "nested bool with NA"
            ( vector_of_optlist bool [ None; Some true; Some false; Some false; Some true ]
            , "x <- F; y <- c(T, F, x, T); c(NA, y)" )
        ; test_eval "single string" (vector_of str "hello", {|"hello"|})
        ; test_eval "multiple string"
            (vector_of_list str [ "abc"; "def"; "ghi" ], {|c("abc", "def", "ghi")|})
        ; test_eval "nested string"
            ( vector_of_list str [ "abc"; "def"; "ghi"; "jkl" ]
            , {|x <- c("ghi"); y <- c("def", x); c("abc", y, "jkl")|} )
        ; test_eval "nested string with empty string vector"
            ( vector_of_list str [ "abc"; "def"; "jkl" ]
            , {|x <- "ghi"[0]; y <- c("def", x); c("abc", y, "jkl")|} )
        ; test_eval "nested string with NA"
            ( vector_of_optlist str [ None; Some "def"; Some "ghi"; Some "jkl" ]
            , {|x <- c("ghi"); y <- c("def", x); c(NA_character_, y, "jkl")|} )
        ] )
    ; ( "combine.mixed"
      , [ test_eval "int with bool" (vector_of_list int [ 0; 1; 0 ], "c(0, T, F)")
        ; test_eval "bool with int" (vector_of_list int [ 0; 1; 1; 0 ], "c(F, T, 1, F)")
        ; test_eval "str with bool"
            (vector_of_list str [ "abc"; "def"; "TRUE" ], {|c("abc", "def", T)|})
        ; test_eval "bool with wtr"
            (vector_of_list str [ "FALSE"; "abc"; "def"; "TRUE" ], {|c(F, "abc", "def", T)|})
        ; test_eval "str with int" (vector_of_list str [ "abc"; "def"; "3" ], {|c("abc", "def", 3)|})
        ; test_eval "int with str"
            (vector_of_list str [ "1"; "abc"; "def"; "3" ], {|c(1, "abc", "def", 3)|})
        ; test_eval "int with str with bool"
            (vector_of_list str [ "1"; "abc"; "TRUE"; "def"; "3" ], {|c(1, "abc", T, "def", 3)|})
        ; test_eval "int with str with bool with NA"
            ( vector_of_optlist str [ Some "1"; Some "abc"; Some "TRUE"; None; Some "def"; Some "3" ]
            , {|c(1, "abc", T, NA, "def", 3)|} )
        ] )
    ; ( "unary.not"
      , [ test_eval "not"
            (vector_of_list bool [ true; false; false; true ], "x <- c(F, T, T, F); !x")
        ; test_eval "nested" (vector_of bool true, "x <- T; y <- !x; !y")
        ; test_eval "coerce"
            (vector_of_list bool [ false; false; true; false; false ], "x <- c(-2, -1, 0, 1, 2); !x")
        ] )
    ; ("unary.not.err", [ test_eval_err "string" (Invalid_argument_type, {|!"abc"|}) ])
    ; ( "unary.plus"
      , [ test_eval "positives" (vector_of_list int (0 -- 4), "x <- c(0, 1, 2, 3, 4); +x")
        ; test_eval "negatives" (vector_of_list int (0 -- ~-4), "x <- c(-0, -1, -2, -3, -4); +x")
        ; test_eval "mixed" (vector_of_list int [ 1; ~-2; ~-3; 4 ], "x <- c(1, -2, -3, 4); +x")
        ; test_eval "empty" (empty_vector T_Int, "x <- 0[0]; +x")
        ; test_eval "NAs"
            ( vector_of_optlist int [ Some 1; None; Some ~-2; Some ~-3; Some 4; None ]
            , "x <- c(1, NA_integer_, -2, -3, 4, NA_integer_); +x" )
        ; test_eval "nested" (vector_of int ~-1, "x <- -1; y <- +x; +y")
        ; test_eval "coerce" (vector_of_list int [ 1; 0 ], "x <- c(T, F); +x")
        ] )
    ; ("unary.plus.err", [ test_eval_err "string" (Invalid_argument_type, {|+"abc"|}) ])
    ; ( "unary.minus"
      , [ test_eval "positives" (vector_of_list int (0 -- ~-4), "x <- 0:4; -x")
        ; test_eval "negatives" (vector_of_list int (0 -- 4), "x <- -0:-4; -x")
        ; test_eval "mixed" (vector_of_list int [ ~-1; 2; 3; ~-4 ], "x <- c(1, -2, -3, 4); -x")
        ; test_eval "empty" (empty_vector T_Int, "x <- 0[0]; -x")
        ; test_eval "NAs"
            ( vector_of_optlist int [ Some ~-1; None; Some 2; Some 3; Some ~-4; None ]
            , "x <- c(1, NA_integer_, -2, -3, 4, NA_integer_); -x" )
        ; test_eval "nested" (vector_of int ~-1, "x <- -1; y <- -x; -y")
        ; test_eval "coerce" (vector_of_list int [ ~-1; 0 ], "x <- c(T, F); -x")
        ] )
    ; ("unary.minus.err", [ test_eval_err "string" (Invalid_argument_type, {|-"abc"|}) ])
    ; ( "unary.as_logical"
      , [ test_eval "bool to bool"
            ( vector_of_optlist bool [ Some true; Some false; Some true; None ]
            , "x <- c(T, F, T, NA); as.logical(x)" )
        ; test_eval "int to bool"
            ( vector_of_optlist bool [ Some true; Some false; Some true; None ]
            , "x <- c(42, 0, -42, NA_integer_); as.logical(x)" )
        ; test_eval "str to bool"
            ( vector_of_optlist bool
                [ Some true
                ; Some false
                ; Some true
                ; Some false
                ; Some true
                ; Some false
                ; Some true
                ; Some false
                ; None
                ; None
                ; None
                ]
            , {|x <- c("T", "F", "TRUE", "FALSE", "True", "False", "true", "false", NA_character_, "tRuE", "abc"); as.logical(x)|}
            )
        ] )
    ; ( "unary.as_integer"
      , [ test_eval "bool to int"
            (vector_of_optlist int [ Some 1; Some 0; None ], "x <- c(T, F, NA); as.integer(x)")
        ; test_eval "int to int"
            ( vector_of_optlist int [ Some ~-2; Some ~-1; Some 0; Some 1; Some 2; None ]
            , "x <- c(-2, -1, 0, 1, 2, NA_integer_); as.integer(x)" )
        ; test_eval "str to int"
            ( vector_of_optlist int [ Some ~-2; Some ~-1; Some 0; Some 1; Some 2; None ]
            , {|x <- c("-2", "-1", "0", "1", "2", NA_character_); as.integer(x)|} )
        ] )
    ; ( "unary.as_integer.err"
      , [ test_eval_err "str to int" (Coercion_introduces_NA, {|x <- "31a"; as.integer(x)|}) ] )
    ; ( "unary.as_character"
      , [ test_eval "bool to str"
            ( vector_of_optlist str [ Some "TRUE"; Some "FALSE"; None ]
            , "x <- c(T, F, NA); as.character(x)" )
        ; test_eval "int to str"
            ( vector_of_optlist str [ Some "-2"; Some "-1"; Some "0"; Some "1"; Some "2"; None ]
            , "x <- c(-2, -1, 0, 1, 2, NA_integer_); as.character(x)" )
        ; test_eval "str to str"
            ( vector_of_optlist str [ Some "abc"; Some "def"; None ]
            , {|x <- c("abc", "def", NA_character_); as.character(x)|} )
        ] )
    ; ( "unary.is_logical"
      , [ test_eval "bool" (vector_of bool true, "x <- c(T, F); is.logical(x)")
        ; test_eval "int" (vector_of bool false, "x <- c(4, -2); is.logical(x)")
        ; test_eval "str" (vector_of bool false, {|x <- c("abc", "def"); is.logical(x)|})
        ] )
    ; ( "unary.is_integer"
      , [ test_eval "bool" (vector_of bool false, "x <- c(T, F); is.integer(x)")
        ; test_eval "int" (vector_of bool true, "x <- c(4, -2); is.integer(x)")
        ; test_eval "str" (vector_of bool false, {|x <- c("abc", "def"); is.integer(x)|})
        ] )
    ; ( "unary.is_character"
      , [ test_eval "bool" (vector_of bool false, "x <- c(T, F); is.character(x)")
        ; test_eval "int" (vector_of bool false, "x <- c(4, -2); is.character(x)")
        ; test_eval "str" (vector_of bool true, {|x <- c("abc", "def"); is.character(x)|})
        ] )
    ; ( "unary.is_na"
      , [ test_eval "bool NA" (vector_of bool true, "is.na(NA)")
        ; test_eval "int NA" (vector_of bool true, "is.na(NA_integer_)")
        ; test_eval "str NA" (vector_of bool true, "is.na(NA_character_)")
        ; test_eval "bool" (vector_of bool false, "is.na(T)")
        ; test_eval "int" (vector_of bool false, "is.na(42)")
        ; test_eval "str" (vector_of bool false, {|is.na("abc")|})
        ] )
    ; ( "unary.length"
      , [ test_eval "empty" (vector_of int 0, "x <- 0[0]; length(x)")
        ; test_eval "bools" (vector_of int 1, "x <- T; length(x)")
        ; test_eval "ints" (vector_of int 3, "x <- c(1, 2, 3); length(x)")
        ; test_eval "strs" (vector_of int 3, {|x <- c("a", "b", "c"); length(x)|})
        ; test_eval "NAs" (vector_of int 3, "x <- c(1, NA, 3); length(x)")
        ] )
    ; ( "binary.arithmetic"
      , [ test_eval "plus single" (vector_of int 5, "2 + 3")
        ; test_eval "plus vector"
            ( vector_of_optlist int [ Some 6; Some ~-3; Some ~-1; Some ~-3; Some 3; None ]
            , "x <- c(4, 2, -5, -2, 0, NA); y <- c(2, -5, 4, -1, 3, 4); x + y" )
        ; test_eval "plus coerce 1"
            (vector_of_list int [ 2; 3; 3; 4; 5 ], "x <- 1:5; y <- c(T, T, F, F, F); x + y")
        ; test_eval "plus coerce 2"
            (vector_of_list int [ 2; 3; 3; 4; 5 ], "x <- 1:5; y <- c(T, T, F, F, F); y + x")
        ; test_eval "minus single" (vector_of int ~-1, "2 - 3")
        ; test_eval "minus vector"
            ( vector_of_optlist int [ Some 2; Some 7; Some ~-9; Some ~-1; Some ~-3; None ]
            , "x <- c(4, 2, -5, -2, 0, NA); y <- c(2, -5, 4, -1, 3, 4); x - y" )
        ; test_eval "minus coerce 1"
            (vector_of_list int [ 0; 1; 3; 4; 5 ], "x <- 1:5; y <- c(T, T, F, F, F); x - y")
        ; test_eval "minus coerce 2"
            (vector_of_list int [ 0; ~-1; ~-3; ~-4; ~-5 ], "x <- 1:5; y <- c(T, T, F, F, F); y - x")
        ; test_eval "times single" (vector_of int 6, "2 * 3")
        ; test_eval "times vector"
            ( vector_of_optlist int [ Some 8; Some ~-10; Some ~-20; Some 2; Some 0; None ]
            , "x <- c(4, 2, -5, -2, 0, NA); y <- c(2, -5, 4, -1, 3, 4); x * y" )
        ; test_eval "times coerce 1"
            (vector_of_list int [ 1; 2; 0; 0; 0 ], "x <- 1:5; y <- c(T, T, F, F, F); x * y")
        ; test_eval "times coerce 2"
            (vector_of_list int [ 1; 2; 0; 0; 0 ], "x <- 1:5; y <- c(T, T, F, F, F); y * x")
        ; test_eval "divide single" (vector_of int 2, "6 / 3")
        ; test_eval "divide vector"
            ( vector_of_optlist int [ Some 4; Some ~-6; Some ~-1; None; None; None ]
            , "x <- c(12, -11, 10, NA, -1, 1); y <- c(3, 2, -10, 1, NA, 0); x / y" )
        ; test_eval "divide coerce 1"
            ( vector_of_optlist int [ Some 1; Some 2; None; None; None ]
            , "x <- 1:5; y <- c(T, T, F, F, F); x / y" )
        ; test_eval "divide coerce 2"
            (vector_of_list int [ 1; 0; 0; 0; 0 ], "x <- 1:5; y <- c(T, T, F, F, F); y / x")
        ; test_eval "modulo single" (vector_of int 1, "6 % 5")
        ; test_eval "modulo vector"
            ( vector_of_optlist int
                [ Some 0; Some 2; Some 1; Some 1; Some ~-1; Some ~-1; None; None; None ]
            , "x <- c(12, 12, 5, -5, 5, -5, NA, 1, 1); y <- c(3, 5, 2, 2, -2, -2, 1, NA, 0); x % y"
            )
        ; test_eval "modulo coerce 1"
            ( vector_of_optlist int [ Some 0; Some 0; None; None; None ]
            , "x <- 1:5; y <- c(T, T, F, F, F); x % y" )
        ; test_eval "modulo coerce 2"
            (vector_of_list int [ 0; 1; 0; 0; 0 ], "x <- 1:5; y <- c(T, T, F, F, F); y % x")
        ] )
    ; ( "binary.relational.str"
      , [ test_eval "less"
            ( vector_of_optlist bool
                [ Some false
                ; Some false
                ; Some false
                ; Some false
                ; Some false
                ; Some false
                ; Some true
                ; None
                ; None
                ]
            , {|x <- c("b", "b", "bbb", "bbb", 10, "", "", NA, "");
                y <- c("a", "b", "bba", "b", "1", "", "0", "x", NA);
                x < y|}
            )
        ; test_eval "less equal"
            ( vector_of_optlist bool
                [ Some false
                ; Some true
                ; Some false
                ; Some false
                ; Some false
                ; Some true
                ; Some true
                ; None
                ; None
                ]
            , {|x <- c("b", "b", "bbb", "bbb", 10, "", "", NA, "");
                y <- c("a", "b", "bba", "b", 1, "", 0, "x", NA);
                x <= y|}
            )
        ; test_eval "greater"
            ( vector_of_optlist bool
                [ Some true
                ; Some false
                ; Some true
                ; Some true
                ; Some true
                ; Some false
                ; Some false
                ; None
                ; None
                ]
            , {|x <- c("b", "b", "bbb", "bbb", 10, "", "", NA, "");
                y <- c("a", "b", "bba", "b", 1, "", 0, "x", NA);
                x > y|}
            )
        ; test_eval "greater equal"
            ( vector_of_optlist bool
                [ Some true
                ; Some true
                ; Some true
                ; Some true
                ; Some true
                ; Some true
                ; Some false
                ; None
                ; None
                ]
            , {|x <- c("b", "b", "bbb", "bbb", 10, "", "", NA, "");
                y <- c("a", "b", "bba", "b", 1, "", 0, "x", NA);
                x >= y|}
            )
        ; test_eval "equal"
            ( vector_of_optlist bool
                [ Some true; Some false; Some false; Some true; None; Some false ]
            , {|x <- c("a", "b", "", "", NA, 10); y <- c("a", "a", "d", "", "x", 1); x == y|} )
        ; test_eval "not equal"
            ( vector_of_optlist bool
                [ Some false; Some true; Some true; Some false; None; Some true ]
            , {|x <- c("a", "b", "", "", NA, 10); y <- c("a", "a", "d", "", "x", 1); x != y|} )
        ; test_eval "coerce 1" (vector_of bool true, {|x <- 1; y <- "1"; x == y|})
        ; test_eval "coerce 2" (vector_of bool true, {|x <- 1; y <- "1"; y == x|})
        ] )
    ; ( "binary.relational.int"
      , [ test_eval "less"
            ( vector_of_optlist bool
                [ Some false
                ; Some false
                ; Some true
                ; Some true
                ; Some false
                ; Some false
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x < y" )
        ; test_eval "less equal"
            ( vector_of_optlist bool
                [ Some false
                ; Some true
                ; Some true
                ; Some true
                ; Some true
                ; Some false
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x <= y"
            )
        ; test_eval "greater"
            ( vector_of_optlist bool
                [ Some true
                ; Some false
                ; Some false
                ; Some false
                ; Some false
                ; Some true
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x > y" )
        ; test_eval "greater equal"
            ( vector_of_optlist bool
                [ Some true
                ; Some true
                ; Some false
                ; Some false
                ; Some true
                ; Some true
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x >= y"
            )
        ; test_eval "equal"
            ( vector_of_optlist bool
                [ Some false
                ; Some true
                ; Some false
                ; Some false
                ; Some true
                ; Some false
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x == y"
            )
        ; test_eval "not equal"
            ( vector_of_optlist bool
                [ Some true
                ; Some false
                ; Some true
                ; Some true
                ; Some false
                ; Some true
                ; None
                ; None
                ; None
                ]
            , "x <- c(0, 0, 0, -5, 0, T, F, NA, NA); y <- c(-4, 0, 2, 0, 0, 0, NA, 0, NA); x != y"
            )
        ; test_eval "coerce 1" (vector_of bool true, {|x <- 1; y <- T; x == y|})
        ; test_eval "coerce 2" (vector_of bool true, {|x <- 1; y <- T; y == x|})
        ] )
    ; ( "binary.relational.bool"
      , [ test_eval "less"
            ( vector_of_optlist bool
                [ Some false; Some false; None; Some true; Some false; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x < y" )
        ; test_eval "less equal"
            ( vector_of_optlist bool
                [ Some true; Some false; None; Some true; Some true; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x <= y"
            )
        ; test_eval "greater"
            ( vector_of_optlist bool
                [ Some false; Some true; None; Some false; Some false; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x > y" )
        ; test_eval "greater equal"
            ( vector_of_optlist bool
                [ Some true; Some true; None; Some false; Some true; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x >= y"
            )
        ; test_eval "equal"
            ( vector_of_optlist bool
                [ Some true; Some false; None; Some false; Some true; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x == y"
            )
        ; test_eval "not equal"
            ( vector_of_optlist bool
                [ Some false; Some true; None; Some true; Some false; None; None; None; None ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x != y"
            )
        ] )
    ; ( "binary.logical"
      , [ test_eval "and 1" (vector_of bool true, "T && T")
        ; test_eval "and 2" (vector_of bool false, "T && F")
        ; test_eval "and 3" (vector_of_na bool, "T && NA")
        ; test_eval "and 4" (vector_of bool false, "F && T")
        ; test_eval "and 5" (vector_of bool false, "F && F")
        ; test_eval "and 6" (vector_of bool false, "F && NA")
        ; test_eval "and 7" (vector_of_na bool, "NA && T")
        ; test_eval "and 8" (vector_of bool false, "NA && F")
        ; test_eval "and 9" (vector_of_na bool, "NA && NA")
        ; test_eval "and 10" (vector_of bool false, "x <- c(F, F); y <- c(T, T); x && y")
        ; test_eval "and empty 1" (vector_of_na bool, "x <- T[0]; y <- F[0]; x && y")
        ; test_eval "and empty 1" (vector_of_na bool, "x <- T[0]; y <- F[0]; y && x")
        ; test_eval "and coerce 1" (vector_of bool true, "1 && -2")
        ; test_eval "and coerce 2" (vector_of bool false, "1 && 0")
        ; test_eval "elementwise and"
            ( vector_of_optlist bool
                [ Some true
                ; Some false
                ; None
                ; Some false
                ; Some false
                ; Some false
                ; None
                ; Some false
                ; None
                ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x & y" )
        ; test_eval "or 1" (vector_of bool true, "T || T")
        ; test_eval "or 2" (vector_of bool true, "T || F")
        ; test_eval "or 3" (vector_of bool true, "T || NA")
        ; test_eval "or 4" (vector_of bool true, "F || T")
        ; test_eval "or 5" (vector_of bool false, "F || F")
        ; test_eval "or 6" (vector_of_na bool, "F || NA")
        ; test_eval "or 7" (vector_of bool true, "NA || T")
        ; test_eval "or 8" (vector_of_na bool, "NA || F")
        ; test_eval "or 9" (vector_of_na bool, "NA || NA")
        ; test_eval "or 10" (vector_of bool true, "x <- c(F, F); y <- c(T, T); x || y")
        ; test_eval "and empty 1" (vector_of_na bool, "x <- T[0]; y <- F[0]; x || y")
        ; test_eval "and empty 1" (vector_of_na bool, "x <- T[0]; y <- F[0]; y || x")
        ; test_eval "or coerce 1" (vector_of bool true, "1 || -2")
        ; test_eval "or coerce 2" (vector_of bool true, "1 || 0")
        ; test_eval "elementwise or"
            ( vector_of_optlist bool
                [ Some true
                ; Some true
                ; Some true
                ; Some true
                ; Some false
                ; None
                ; Some true
                ; None
                ; None
                ]
            , "x <- c(T, T, T, F, F, F, NA, NA, NA); y <- c(T, F, NA, T, F, NA, T, F, NA); x | y" )
        ] )
    ; ( "binary.seq"
      , [ test_eval "seq 1" (vector_of_list int (1 -- 5), "1:5")
        ; test_eval "seq 2" (vector_of_list int (5 -- 1), "5:1")
        ; test_eval "seq 3" (vector_of_list int (~-1 -- ~-5), "-1:-5")
        ; test_eval "seq 4" (vector_of_list int (~-5 -- ~-1), "-5:-1")
        ; test_eval "seq 5" (vector_of_list int (~-4 -- 1), "-4:1")
        ; test_eval "seq 6" (vector_of_list int (4 -- ~-1), "4:-1")
        ; test_eval "seq 7" (vector_of_list int (0 -- 1), "F:T")
        ; test_eval "seq 8" (vector_of_list int (~-4 -- 1), {|"-4":"1"|})
        ; test_eval "seq 9" (vector_of_list int (1 -- 5), "x <- 1; y <- 5; x:y")
        ] )
    ; ( "binary.err"
      , [ test_eval_err "vector length mismatch 1"
            (Vector_lengths_do_not_match, "x <- 1:5; y <- 1:4; x + y")
        ; test_eval_err "vector length mismatch 2" ~is_valid_r:true
            (Vector_lengths_do_not_match, {|x <- "a"; y <- c("a", "b"); x < y|})
        ; test_eval_err "vector length mismatch 3"
            (Vector_lengths_do_not_match, "x <- 1:5; y <- 1:4; x < y")
        ; test_eval_err "vector length mismatch 4"
            (Vector_lengths_do_not_match, "x <- c(1, 2); y <- 1; x:y")
        ; test_eval_err "invalid argument type 1"
            (Invalid_argument_type, {|x <- 1:2; y <- c("4", "3"); x + y|})
        ; test_eval_err "invalid argument type 2"
            (Invalid_argument_type, {|x <- 1:2; y <- c("4", "3"); y + x|})
        ; test_eval_err "invalid argument type 3"
            (Invalid_argument_type, {|x <- c(T, T); y <- c("4", "3"); x && y|})
        ; test_eval_err "invalid argument type 4"
            (Invalid_argument_type, {|x <- c(T, T); y <- c("4", "3"); y && x|})
        ; test_eval_err "invalid seq arg 1" (Expected_scalar, "x <- c(1, 2); y <- c(4, 5); x:y")
        ; test_eval_err "invalid seq arg 2" (Expected_scalar, "x <- c(1, 2); y <- c(4, 5); y:x")
        ; test_eval_err "invalid seq arg 3" (Expected_scalar, "x <- 1[0]; y <- 4[0]; x:y")
        ; test_eval_err "invalid seq arg 4" (Expected_scalar, "x <- 1[0]; y <- 4[0]; y:x")
        ; test_eval_err "invalid seq arg 5" (Coercion_introduces_NA, {|x <- "1"; y <- "33a"; x:y|})
        ; test_eval_err "invalid seq arg 6" (Coercion_introduces_NA, {|x <- "1"; y <- "33a"; y:x|})
        ; test_eval_err "invalid seq arg 7" (NA_not_allowed, "NA:1")
        ; test_eval_err "invalid seq arg 8" (NA_not_allowed, "1:NA")
        ; test_eval_err "invalid seq arg 9" (NA_not_allowed, "NA:NA")
        ] )
    ; ( "subset1_nothing"
      , [ test_eval "int" (vector_of int 2, "2[]")
        ; test_eval "empty int" (empty_vector T_Int, "x <- 0[0]; x[]")
        ; test_eval "multiple ints" (vector_of_list int [ 3; 1; 4 ], "x <- c(3, 1, 4); x[]")
        ; test_eval "ints with NAs"
            ( vector_of_optlist int [ Some 5; None; Some 6; Some 7; None ]
            , "x <- c(5, NA, 6, 7, NA); x[]" )
        ; test_eval "empty bool" (empty_vector T_Bool, "x <- T[0]; x[]")
        ; test_eval "empty string" (empty_vector T_Str, {|x <- "abc"[0]; x[]|})
        ; test_eval "coerce" (vector_of_list int [ 0; 0 ], "x <- c(0, F); x[]")
        ] )
    ; ( "subset1.logical"
      , [ test_eval "int vector 1"
            (vector_of_list int [ 1; 4 ], "x <- 1:4; y <- c(T, F, F, T); x[y]")
        ; test_eval "int vector 2"
            (vector_of_list int [ 1; 2; 3; 4 ], "x <- 1:4; y <- c(T, T, T, T); x[y]")
        ; test_eval "int vector 3" (empty_vector T_Int, "x <- 1:4; y <- c(F, F, F, F); x[y]")
        ; test_eval "int vector with NA index"
            (vector_of_optlist int [ Some 1; None; Some 4 ], "x <- 1:4; y <- c(T, NA, F, T); x[y]")
        ] )
    ; ( "subset1.zero"
      , [ test_eval "int vector 1" (empty_vector T_Int, "7[0]")
        ; test_eval "int vector 2" (empty_vector T_Int, "x <- 7:5; x[0]")
        ; test_eval "int vector 3" (empty_vector T_Int, "x <- 7:5; y <- c(0); x[y]")
        ; test_eval "empty int vector" (empty_vector T_Int, "x <- 0[0]; x[0]")
        ; test_eval "NA int vector" (empty_vector T_Int, "x <- c(NA_integer_, NA, NA); x[0]")
        ] )
    ; ( "subset1.positive"
      , [ test_eval "index 1" (vector_of int 11, "x <- 11:14; x[1]")
        ; test_eval "index 2" (vector_of int 12, "x <- 11:14; x[2]")
        ; test_eval "index 3" (vector_of int 13, "x <- 11:14; x[3]")
        ; test_eval "index 4" (vector_of int 14, "x <- 11:14; x[4]")
        ; test_eval "index 5" (vector_of int 13, "x <- 11:14; y <- 1:4; z <- y[3]; x[z]")
        ; test_eval "index 6 out-of-bounds" (vector_of_na int, "x <- 11:14; x[5]")
        ; test_eval "index 7 NA" (vector_of_na int, "x <- 11:14; x[NA_integer_]")
        ; test_eval "multiple indexes"
            (vector_of_list int [ 11; 13 ], "x <- 11:14; y <- c(1, 3); x[y]")
        ; test_eval "multiple indexes and ignored zero"
            (vector_of_list int [ 11; 13 ], "x <- 11:14; y <- c(0, 1, 3); x[y]")
        ; test_eval "multiple indexes and out-of-bounds"
            (vector_of_optlist int [ Some 11; Some 13; None ], "x <- 11:14; y <- c(1, 3, 5); x[y]")
        ; test_eval "multiple indexes repeated"
            (vector_of_list int [ 13; 13; 12; 14; 13 ], "x <- 11:14; y <- c(3, 3, 2, 4, 3); x[y]")
        ; test_eval "multiple indexes with NA"
            (vector_of_optlist int [ Some 12; None; Some 11 ], "x <- 11:14; y <- c(2, NA, 1); x[y]")
        ] )
    ; ( "subset1.err"
      , [ test_eval_err "vector lengths do not match 1" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:4; x[T]")
        ; test_eval_err "vector lengths do not match 2" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:4; y <- c(T, F); x[y]")
        ; test_eval_err "negative subscripts 1" ~is_valid_r:true
            (Invalid_subset_index, "x <- 1:4; x[-1]")
        ; test_eval_err "negative subscripts 2"
            (Invalid_subset_index, "x <- 1:4; y <- c(1, -1); x[y]")
        ; test_eval_err "string subscript" ~is_valid_r:true
            (Invalid_argument_type, {|x <- 1:4; x["1"]|})
        ] )
    ; ( "subset2"
      , [ test_eval "index 1" (vector_of int 1, "x <- c(1, 2, 3, NA); x[[1]]")
        ; test_eval "index 2" (vector_of int 2, "x <- c(1, 2, 3, NA); x[[2]]")
        ; test_eval "index 3" (vector_of int 3, "x <- c(1, 2, 3, NA); x[[3]]")
        ; test_eval "index 4" (vector_of_na int, "x <- c(1, 2, 3, NA); x[[4]]")
        ; test_eval "mixed types" (vector_of int 1, "x <- c(1, 2, 3); x[[T]]")
        ] )
    ; ( "subset2.err"
      , [ test_eval_err "index 0" (Invalid_subset_index, "x <- 1:3; x[[0]]")
        ; test_eval_err "invalid subset type" (Invalid_subset_index, {|x <- 1:3; x[["1"]]|})
        ; test_eval_err "empty vector as index" (Invalid_subset_index, "x <- 1:3; y <- 1[0]; x[[y]]")
        ; test_eval_err "single negative index" (Invalid_subset_index, "x <- 1:3; x[[-1]]")
        ; test_eval_err "multiple index 1" (Invalid_subset_index, "x <- 1:3; y <- 1:2; x[[y]]")
        ; test_eval_err "out-of-bounds 1" (Invalid_subset_index, "x <- 1:3; x[[4]]")
        ; test_eval_err "out-of-bounds 2" (Invalid_subset_index, "x <- 1:3; x[[-4]]")
        ; test_eval_err "out-of-bounds 3" (Invalid_subset_index, "x <- 1[0]; x[[1]]")
        ; test_eval_err "out-of-bounds with NA" (Invalid_subset_index, "x <- 1:4; x[[NA]]")
        ] )
    ; ( "subset1_nothing_assign"
      , [ test_eval "same type"
            (vector_of_list int [ 0; 0; 0 ], "x <- 1:3; y <- c(0, 0, 0); x[] <- y; x")
        ; test_eval "different type 1"
            (vector_of_list int (1 -- 3), "x <- c(T, T, F); y <- 1:3; x[] <- y; x")
        ; test_eval "different type 2"
            (vector_of_list int [ 1; 1; 0 ], "x <- 1:3; y <- c(T, T, F); x[] <- y; x")
        ; test_eval "different type 3"
            ( vector_of_list str [ "abc"; "def"; "ghi" ]
            , {|x <- c(T, T, F); y <- c("abc", "def", "ghi"); x[] <- y; x|} )
        ; test_eval "different type 4"
            ( vector_of_list str [ "TRUE"; "TRUE"; "FALSE" ]
            , {|x <- c("abc", "def", "ghi"); y <- c(T, T, F); x[] <- y; x|} )
        ; test_eval "different type 5"
            ( vector_of_list str [ "abc"; "def"; "ghi" ]
            , {|x <- 1:3; y <- c("abc", "def", "ghi"); x[] <- y; x|} )
        ; test_eval "different type 6"
            ( vector_of_list str [ "1"; "2"; "3" ]
            , {|x <- c("abc", "def", "ghi"); y <- 1:3; x[] <- y; x|} )
        ; test_eval "NA"
            ( vector_of_optlist int [ None; None; None ]
            , "x <- 11:13; y <- c(NA, NA, NA); x[] <- y; x" )
        ; test_eval "return value 1"
            (vector_of_list int (1 -- 3), "x <- c(T, T, F); y <- 1:3; x[] <- y")
        ; test_eval "return value 2"
            (vector_of_list bool [ true; true; false ], "x <- 1:3; y <- c(T, T, F); x[] <- y")
        ; test_eval "return value 3"
            ( vector_of_list str [ "abc"; "def"; "ghi" ]
            , {|x <- c(T, T, F); y <- c("abc", "def", "ghi"); x[] <- y|} )
        ; test_eval "return value 4"
            ( vector_of_list bool [ true; true; false ]
            , {|x <- c("abc", "def", "ghi"); y <- c(T, T, F); x[] <- y|} )
        ; test_eval "return value 5"
            ( vector_of_list str [ "abc"; "def"; "ghi" ]
            , {|x <- 1:3; y <- c("abc", "def", "ghi"); x[] <- y|} )
        ; test_eval "return value 6"
            (vector_of_list int [ 1; 2; 3 ], {|x <- c("abc", "def", "ghi"); y <- 1:3; x[] <- y|})
        ; test_eval "return value 7"
            (vector_of_optlist bool [ None; None; None ], "x <- 11:13; y <- c(NA, NA, NA); x[] <- y")
        ] )
    ; ( "subset1_nothing_assign.err"
      , [ test_eval_err "wrong length 1" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 11:13; x[] <- 1")
        ; test_eval_err "wrong length 2"
            (Vector_lengths_do_not_match, "x <- 11:13; y <- 1:2; x[] <- y")
        ; test_eval_err "wrong length 3"
            (Vector_lengths_do_not_match, "x <- 11:13; y <- 1:4; x[] <- y")
        ; test_eval_err "wrong length 4"
            (Vector_lengths_do_not_match, "x <- 11:13; y <- 1:6; x[] <- y")
        ; test_eval_err "wrong length 5"
            (Vector_lengths_do_not_match, "x <- 11:13; y <- 1[0]; x[] <- y")
        ; test_eval_err "not found" (object_not_found "x", "x[] <- 1")
        ] )
    ; ( "subset1_assign.logical"
      , [ test_eval "assignment 1"
            ( vector_of_list int [ 1; 12; 13; 14; 5 ]
            , "x <- 1:5; i <- c(F, T, T, T, F); y <- 12:14; x[i] <- y; x" )
        ; test_eval "assignment 2"
            (vector_of_list int [ 9; 8; 3 ], "x <- 1:3; i <- c(T, T, F); y <- 9:8; x[i] <- y; x")
        ; test_eval "coerce 1"
            (vector_of_list int [ 1; 0; 3 ], "x <- 1:3; i <- c(T, T, F); y <- c(T, F); x[i] <- y; x")
        ; test_eval "coerce 2"
            ( vector_of_list str [ "abc"; "def"; "3" ]
            , {|x <- 1:3; i <- c(T, T, F); y <- c("abc", "def"); x[i] <- y; x|} )
        ; test_eval "return value 1"
            (vector_of_list int (12 -- 14), "x <- 1:5; i <- c(F, T, T, T, F); y <- 12:14; x[i] <- y")
        ; test_eval "return value 2"
            (vector_of_list int (9 -- 8), "x <- 1:3; i <- c(T, T, F); y <- 9:8; x[i] <- y")
        ; test_eval "return value 3"
            ( vector_of_list bool [ true; false ]
            , "x <- 1:3; i <- c(T, T, F); y <- c(T, F); x[i] <- y" )
        ; test_eval "return value 4"
            ( vector_of_list str [ "abc"; "def" ]
            , {|x <- 1:3; i <- c(T, T, F); y <- c("abc", "def"); x[i] <- y|} )
        ] )
    ; ( "subset1_assign.logical.err"
      , [ test_eval_err "NA in index 1"
            (Invalid_subset_index, "x <- 1:5; i <- c(T, F, NA, T, F); y <- 1:2; x[i] <- y")
        ; test_eval_err "NA in index 2" ~is_valid_r:true
            (* R allows this because the RHS has only one element *)
            (Invalid_subset_index, "x <- 1:5; i <- c(T, F, NA, T, F); x[i] <- 0")
        ; test_eval_err "wrong index length 1" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:5; x[T] <- 0")
        ; test_eval_err "wrong index length 2" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:5; i <- c(T, F); x[i] <- 0")
        ; test_eval_err "wrong index length 3" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:5; i <- c(T, F, F, T, T, F); x[i] <- 0")
        ; test_eval_err "wrong index length 4" ~is_valid_r:true
            (Vector_lengths_do_not_match, "x <- 1:5; i <- c(T, T, T, T, T, T); x[i] <- 0")
        ; test_eval_err "wrong replacement length 1" ~is_valid_r:true
            (Invalid_subset_replacement, "x <- 1:5; i <- c(T, F, F, T, T); y <- 0; x[i] <- y")
        ; test_eval_err "wrong replacement length 2"
            (Invalid_subset_replacement, "x <- 1:5; i <- c(T, F, F, T, T); y <- 11:12; x[i] <- y")
        ; test_eval_err "wrong replacement length 3"
            (Invalid_subset_replacement, "x <- 1:5; i <- c(T, F, F, T, T); y <- 11:14; x[i] <- y")
        ; test_eval_err "wrong replacement length 4"
            (Invalid_subset_replacement, "x <- 1:5; i <- c(T, F, F, T, T); y <- 1[0]; x[i] <- y")
        ; test_eval_err "not found" (object_not_found "x", "x[T] <- 1")
        ] )
    ; ( "subset1_assign.zero"
      , [ test_eval "int vector 1" (vector_of int 7, "x <- 7; x[0] <- 42; x")
        ; test_eval "int vector 2" (vector_of int 7, "x <- 7; x[0] <- NA; x")
        ; test_eval "empty int vector" (empty_vector T_Int, "x <- 0[0]; x[0] <- 1; x")
        ; test_eval "empty index vector" (vector_of int 7, "x <- 7; y <- 0[0]; x[y] <- 1; x")
        ; test_eval "coerce 1" (vector_of int 7, "x <- 7; x[0] <- T; x")
        ; test_eval "coerce 2" (vector_of str "7", {|x <- 7; x[0] <- "foobar"; x|})
        ; test_eval "return value 1" (vector_of int 42, "x <- 7; x[0] <- 42")
        ; test_eval "return value 2" (vector_of_na bool, "x <- 7; x[0] <- NA")
        ; test_eval "return value 3" (vector_of int 1, "x <- 0[0]; x[0] <- 1")
        ; test_eval "return value 4" (vector_of int 1, "x <- 7; y <- 0[0]; x[y] <- 1")
        ; test_eval "return value 5" (vector_of bool true, "x <- 7; x[0] <- T")
        ; test_eval "return value 6" (vector_of str "foobar", {|x <- 7; x[0] <- "foobar"|})
        ] )
    ; ( "subset1_assign.zero.err"
      , [ test_eval_err "NA in index" ~is_valid_r:true
            (* R allows this because the RHS has only one element *)
            (Invalid_subset_index, "x <- 1:4; i <- c(0, NA); x[i] <- 0")
        ; test_eval_err "not found" (object_not_found "x", "x[0] <- 1")
        ] )
    ; ( "subset1_assign.positive"
      , [ test_eval "single index" (vector_of_list int [ 0; 12; 13; 14 ], "x <- 11:14; x[1] <- 0; x")
        ; test_eval "single index extension 1"
            ( vector_of_optlist int [ Some 11; Some 12; Some 13; Some 14; None; None; None; Some 0 ]
            , "x <- 11:14; x[8] <- 0; x" )
        ; test_eval "single index extension 2"
            ( vector_of_optlist bool [ Some true; Some false; None; None; None; Some true ]
            , "x <- c(T, F); x[6] <- T; x" )
        ; test_eval "single index extension 3"
            ( vector_of_optlist str [ Some "abc"; Some "def"; Some "ghi"; None; None; Some "xyz" ]
            , {|x <- c("abc", "def", "ghi"); x[6] <- "xyz"; x|} )
        ; test_eval "multiple indexes"
            (vector_of_list int [ 9; 8; 13; 14 ], "x <- 11:14; i <- 1:2; y <- 9:8; x[i] <- y; x")
        ; test_eval "multiple indexes with repeats 1"
            ( vector_of_list int [ 8; 7; 13; 14 ]
            , "x <- 11:14; i <- c(1, 1, 2); y <- 9:7; x[i] <- y; x" )
        ; test_eval "multiple indexes with repeats 2"
            ( vector_of_list int [ 8; 7; 6; 14; 15; 16 ]
            , "x <- 11:16; i <- c(1, 1, 2, 3); y <- 9:6; x[i] <- y; x" )
        ; test_eval "multiple indexes extension"
            ( vector_of_list int [ 0; 12; 9; 8 ]
            , "x <- 11:12; i <- c(1, 3, 4); y <- c(0, 9, 8); x[i] <- y; x" )
        ; test_eval "multiple indexes with zero"
            ( vector_of_list int [ 8; 9; 13; 14 ]
            , "x <- 11:14; i <- c(2, 0, 1); y <- 9:8; x[i] <- y; x" )
        ; test_eval "coerce 1"
            ( vector_of_list int [ 0; 0; 0; 14 ]
            , "x <- 11:14; i <- 1:3; y <- c(F, F, F); x[i] <- y; x" )
        ; test_eval "coerce 2"
            ( vector_of_list str [ "a"; "b"; "c"; "14" ]
            , {|x <- 11:14; i <- 1:3; y <- c("a", "b", "c"); x[i] <- y; x|} )
        ; test_eval "return value 1"
            (vector_of_list int [ 9; 8 ], "x <- 11:14; i <- 1:2; y <- 9:8; x[i] <- y")
        ; test_eval "return value 2"
            ( vector_of_list int [ 0; 9; 8 ]
            , "x <- 11:12; i <- c(1, 3, 4); y <- c(0, 9, 8); x[i] <- y" )
        ; test_eval "return value 3"
            (vector_of_list int [ 9; 8 ], "x <- 11:16; i <- 1:2; y <- 9:8; x[i] <- y")
        ; test_eval "return value 4"
            ( vector_of_list bool [ false; false; false ]
            , "x <- 11:14; i <- 1:3; y <- c(F, F, F); x[i] <- y" )
        ; test_eval "return value 5"
            ( vector_of_list str [ "a"; "b"; "c" ]
            , {|x <- 11:14; i <- 1:3; y <- c("a", "b", "c"); x[i] <- y|} )
        ] )
    ; ( "subset1_assign.positive.err"
      , [ test_eval_err "NA index 1" ~is_valid_r:true
            (* R allows this because the RHS has only one element *)
            (Invalid_subset_index, "x <- 1:4; x[NA_integer_] <- 0")
        ; test_eval_err "NA index 2"
            (Invalid_subset_index, "x <- 1:4; y <- 10:11; x[NA_integer_] <- y")
        ; test_eval_err "NA index 3" ~is_valid_r:true
            (* R allows this because the RHS has only one element *)
            (Invalid_subset_index, "x <- 1:4; i <- c(1, NA); x[i] <- 10")
        ; test_eval_err "NA index 4"
            (Invalid_subset_index, "x <- 1:4; i <- c(1, NA); y <- 10:11; x[i] <- y")
        ; test_eval_err "negative index 1" ~is_valid_r:true
            (Invalid_subset_index, "x <- 1:4; i <- -1; y <- 9:7; x[i] <- y")
        ; test_eval_err "negative index 2" ~is_valid_r:true
            (Invalid_subset_index, "x <- 1:4; i <- c(-1, -2); y <- 9:8; x[i] <- y")
        ; test_eval_err "negative index 3"
            (Invalid_subset_index, "x <- 1:4; i <- c(1, -1); y <- 9:8; x[i] <- y")
        ; test_eval_err "empty replacement"
            (Invalid_subset_replacement, "x <- 1:4; y <- 1[0]; x[1] <- y")
        ; test_eval_err "wrong replacement length 1"
            (Invalid_subset_replacement, "x <- 11:14; i <- 1:2; y <- 9:7; x[i] <- y")
        ; test_eval_err "wrong replacement length 2"
            (Invalid_subset_replacement, "x <- 11:14; i <- c(1, 1, 3); y <- 9:8; x[i] <- y")
        ; test_eval_err "wrong replacement length 3"
            (Invalid_subset_replacement, "x <- 11:14; i <- 1:3; y <- 9:8; x[i] <- y")
        ; test_eval_err "wrong replacement length 4"
            (Invalid_subset_replacement, "x <- 11:14; i <- c(2, 0, 1); y <- 9:7; x[i] <- y")
        ; test_eval_err "not found" (object_not_found "x", "x[1] <- 1")
        ] )
    ; ( "subset2_assign"
      , [ test_eval "int vector 1"
            (vector_of_list int [ 9; 12; 13; 14 ], "x <- 11:14; x[[1]] <- 9; x")
        ; test_eval "int vector 2"
            (vector_of_list int [ 11; 12; 13; 9 ], "x <- 11:14; i <- c(4); x[[i]] <- 9; x")
        ; test_eval "extension 1"
            ( vector_of_optlist int [ Some 11; Some 12; Some 13; Some 14; None; None; Some 9 ]
            , "x <- 11:14; x[[7]] <- 9; x" )
        ; test_eval "extension 2" (vector_of_list int [ 9 ], "x <- 1[0]; x[[1]] <- 9; x")
        ; test_eval "coerce 1" (vector_of_list int [ 8; 12; 13; 14 ], "x <- 11:14; x[[T]] <- 8; x")
        ; test_eval "coerce 2"
            (vector_of_list str [ "8"; "12"; "13"; "14" ], {|x <- 11:14; x[[1]] <- "8"; x|})
        ; test_eval "coerce 3"
            ( vector_of_list str [ "8"; "def"; "ghi"; "jkl" ]
            , {|x <- c("abc", "def", "ghi", "jkl"); x[[1]] <- 8; x|} )
        ; test_eval "return value 1" (vector_of int 9, "x <- 11:14; x[[8]] <- 9")
        ; test_eval "return value 2" (vector_of int 9, "x <- 11:14; x[[4]] <- 9")
        ; test_eval "return value 3" (vector_of int 8, "x <- 11:14; x[[T]] <- 8")
        ; test_eval "return value 4" (vector_of str "8", {|x <- 11:14; x[[1]] <- "8"|})
        ; test_eval "return value 5"
            (vector_of int 8, {|x <- c("abc", "def", "ghi", "jkl"); x[[1]] <- 8|})
        ] )
    ; ( "subset2_assign.err"
      , [ test_eval_err "empty vector as index"
            (Invalid_subset_index, "x <- 11:14; i <- 1[0]; x[[i]] <- 9")
        ; test_eval_err "multiple index 1"
            (Invalid_subset_index, "x <- 11:14; i <- 1:2; x[[i]] <- 9")
        ; test_eval_err "multiple index 2"
            (Invalid_subset_index, "x <- 11:14; i <- -1:-2; x[[i]] <- 9")
        ; test_eval_err "string index" ~is_valid_r:true
            (Invalid_subset_index, {|x <- 11:14; x[["1"]] <- 9|})
        ; test_eval_err "replacement vector too long"
            (Invalid_subset_replacement, "x <- 11:14; y <- 9:8; x[[1]] <- y")
        ; test_eval_err "replacement vector too short"
            (Invalid_subset_replacement, "x <- 11:14; y <- 1[0]; x[[1]] <- y")
        ; test_eval_err "index 0" (Invalid_subset_index, "x <- 11:14; x[[0]] <- 9")
        ; test_eval_err "negative index" (Invalid_subset_index, "x <- 11:14; x[[-1]] <- 9")
        ; test_eval_err "NA index" (Invalid_subset_index, "x <- 11:14; x[[NA]] <- 9")
        ; test_eval_err "not found" (object_not_found "x", "x[[1]] <- 1")
        ] )
    ; ( "if"
      , [ test_eval "true branch 1" (vector_of int 1, "if (T) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "true branch 2" (vector_of int 1, "x <- 0; if (T) { x <- 1 }; x")
        ; test_eval "false branch 1" (vector_of int 2, "if (F) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "false branch 2" (vector_of int 0, "x <- 0; if (F) { x <- 1 }; x")
        ; test_eval "variable cond 1"
            (vector_of int 1, "b <- T; if (b) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "variable cond 2"
            (vector_of int 2, "b <- F; if (b) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "coerce 1" (vector_of int 1, "b <- 1; if (b) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "coerce 2" (vector_of int 2, "b <- 0; if (b) { x <- 1 } else { x <- 2 }; x")
        ; test_eval "coerce 3"
            (vector_of int 1, {|b <- "TRUE"; if (b) { x <- 1 } else { x <- 2 }; x|})
        ; test_eval "coerce 4"
            (vector_of int 2, {|b <- "FALSE"; if (b) { x <- 1 } else { x <- 2 }; x|})
        ; test_eval "nested 1"
            ( vector_of int 1
            , {|a <- T; b <- T;
                if (a) {
                  if (b) { x <- 1 } else { x <- 2 }
                } else {
                  if (b) { x <- 3 } else { x <- 4 }
                }; x|}
            )
        ; test_eval "nested 2"
            ( vector_of int 2
            , {|a <- T; b <- F;
                if (a) {
                  if (b) { x <- 1 } else { x <- 2 }
                } else {
                  if (b) { x <- 3 } else { x <- 4 }
                }; x|}
            )
        ; test_eval "nested 3"
            ( vector_of int 3
            , {|a <- F; b <- T;
                if (a) {
                  if (b) { x <- 1 } else { x <- 2 }
                } else {
                  if (b) { x <- 3 } else { x <- 4 }
                }; x|}
            )
        ; test_eval "nested 4"
            ( vector_of int 4
            , {|a <- F; b <- F;
                if (a) {
                  if (b) { x <- 1 } else { x <- 2 }
                } else {
                  if (b) { x <- 3 } else { x <- 4 }
                }; x|}
            )
        ] )
    ; ( "if.err"
      , [ test_eval_err "non-scalar cond 1" (Expected_scalar, "b <- 1:2; if (b) { x <- 1 }")
        ; test_eval_err "non-scalar cond 2" (Expected_scalar, "b <- 1[0]; if (b) { x <- 1 }")
        ; test_eval_err "NA cond" (Missing_value_need_true_false, "b <- NA; if (b) { x <- 1 }")
        ] )
    ; ( "for"
      , [ test_eval "simple"
            ( vector_of int 105
            , {|res <- 1; v <- c(3, 5, 7);
                for (x in v) { res <- res * x };
                res|}
            )
        ; test_eval "if inside for"
            ( vector_of int 30
            , {|res <- 0; v <- 1:10;
               for (x in v) {
                 mod <- x % 2; cond <- mod == 0;
                 if (cond) { res <- res + x }
               }; res|}
            )
        ; test_eval "nested for"
            ( vector_of int 35
            , {|res <- 0; v <- 1:5;
               for (x in v) {
                 u <- 1:x;
                 for (y in u) { res <- res + y }
               }; res|}
            )
        ; test_eval "clobber" (vector_of int 10, "x <- 0; v <- 1:10; for (x in v) { NA }; x")
        ] )
    ; ( "functions"
      , [ test_eval "const 1" (vector_of int 42, "f <- function() { 42 }; f()")
        ; test_eval "const 2" (vector_of int 42, "f <- function() { 42 }; x <- f(); x")
        ; test_eval "id 1" (vector_of int 42, "f <- function(x) { x }; f(42)")
        ; test_eval "id 2" (vector_of int 42, "f <- function(x) { x }; y <- 42; f(y)")
        ; test_eval "id 3" (vector_of int 42, "f <- function(x) { x }; x <- 42; f(x)")
        ; test_eval "id 4" (vector_of int 42, "f <- function(x) { x }; x <- 42; x <- f(x); x")
        ; test_eval "scoping 1" (vector_of int 42, "x <- 42; f <- function(x) { x }; f(0); x")
        ; test_eval "scoping 2" (vector_of int 42, "x <- 42; f <- function() { x <- 0; x }; f(); x")
        ; test_eval "args 1" (vector_of int 42, "f <- function(a, b) { a + b }; f(20, 22)")
        ; test_eval "args 2"
            (vector_of int 42, "f <- function(a, b) { a + b }; x <- 20; y <- 22; f(x, y)")
        ; test_eval "if inside 1"
            ( vector_of int 1
            , {|f <- function(x) {
                if (x) {1} else {0}
              }
              f(T)|}
            )
        ; test_eval "if inside 2"
            ( vector_of int 0
            , {|f <- function(x) {
                if (x) {1} else {0}
              }
              f(F)|}
            )
        ; test_eval "inside if 1"
            ( vector_of int 1
            , {|f <- function(x) { x }
              if (T) { f(1) } else { f(0) } |} )
        ; test_eval "inside if 2"
            ( vector_of int 0
            , {|f <- function(x) { x }
              if (F) { f(1) } else { f(0) } |} )
        ; test_eval "for inside"
            ( vector_of int 55
            , {|f <- function(a,b,c,d,e,f,g,h,i,j) {
               v <- c(a,b,c,d,e,f,g,h,i,j)
               res <- 0
               for (x in v) { res <- res + x }
               res
            }
            f(1,2,3,4,5,6,7,8,9,10)|}
            )
        ; test_eval "inside for"
            ( vector_of int 55
            , {|f <- function(x) { x }
              v <- 1:10
              res <- 0
              for (x in v) {
                y <- f(x)
                res <- res + y
              }
              res|}
            )
        ; test_eval "multiple"
            ( vector_of int 194
            , {|f <- function(x, y) { x + y }
              g <- function(x) { x * x }
              h <- function(x, y) {
                x1 <- g(x); x2 <- g(x1);
                y1 <- g(y); y2 <- g(y1);
                f1 <- f(x2, y2)
                f(f1, f1)
              }
              h(2, 3)|}
            )
        ; test_eval "factorial"
            ( vector_of int 120
            , {|fact <- function(x) {
                cond <- x == 1
                if (cond) {
                  1
                } else {
                  x1 <- x - 1
                  fact1 <- fact(x1)
                  x * fact1
                }
              }
              fact(5)
            |}
            )
        ; test_eval "fib"
            ( vector_of int 55
            , {|fib <- function(x) {
                cond <- x <= 1
                if (cond) {
                  x
                } else {
                  x1 <- x - 1; x2 <- x - 2
                  fib1 <- fib(x1); fib2 <- fib(x2)
                  fib1 + fib2
                }
              }
              fib(10)
            |}
            )
        ] )
    ; ( "functions.err"
      , [ test_eval_err "wrong arg number 1"
            (Invalid_number_of_args { expected = 0; received = 1 }, "f <- function() { 42 }; f(1)")
        ; test_eval_err "wrong arg number 2"
            (Invalid_number_of_args { expected = 1; received = 0 }, "f <- function(x) { x }; f()")
        ; test_eval_err "undefined function" (Function_not_found "f", "f()")
        ; test_eval_err "free variables" ~is_valid_r:true
            (object_not_found ~pre:"f" "x", "x <- 42; f <- function() { x }; f()")
        ] )
    ]

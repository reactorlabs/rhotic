open Expr

let lit_to_r = function
  | Int i -> Int.to_string i ^ "L"
  | Bool b -> if b then "TRUE" else "FALSE"
  | Str s -> "\"" ^ s ^ "\""
  | NA_bool -> "NA"
  | NA_int -> "NA_integer_"
  | NA_str -> "NA_character_"

let rec val_to_r = function
  | Vector (a, t) ->
      (* Need a workaround because rhotic doesn't have the NULL vector, only typed empty vectors. *)
      if Array.length a = 0 then
        match t with
        | T_Bool -> Printf.sprintf "as.logical(NULL)"
        | T_Int -> Printf.sprintf "as.integer(NULL)"
        | T_Str -> Printf.sprintf "as.character(NULL)"
      else
        let inner = a |> Array.map lit_to_r |> Array.to_list |> String.concat ", " in
        Printf.sprintf "c(%s)" inner
  | Dataframe (cols, names) ->
      let inner = Array.map2 (fun v n -> n ^ " = " ^ val_to_r v) cols names |> Array.to_list |> String.concat ", " in
      Printf.sprintf "data.frame(%s)" inner

let to_r stmt_list =
  let simple_expr_to_r = function
    | Lit l -> lit_to_r l
    | Var x -> x in

  let expr_to_r = function
    | Combine es ->
        let inner = es |> List.map simple_expr_to_r |> String.concat ", " in
        Printf.sprintf "c(%s)" inner
    | Dataframe_Ctor bs ->
        let binding_to_r (b, se) = Printf.sprintf "%s = %s" b (simple_expr_to_r se) in
        let inner = bs |> List.map binding_to_r |> String.concat ", " in
        Printf.sprintf "data.frame(%s)" inner
    | Coerce_Op (op, se) -> (
        let coerce op = Printf.sprintf "%s(%s)" op (simple_expr_to_r se) in
        match op with
        | To_Bool -> coerce "as.logical"
        | To_Int -> coerce "as.integer"
        | To_Str -> coerce "as.character" )
    | Unary_Op (op, se) -> (
        let unary op = Printf.sprintf "%s%s" op (simple_expr_to_r se) in
        match op with
        | Logical_Not -> unary "!"
        | Unary_Plus -> unary "+"
        | Unary_Minus -> unary "-" )
    | Binary_Op (op, se1, se2) -> (
        let binary op = Printf.sprintf "%s %s %s" (simple_expr_to_r se1) op (simple_expr_to_r se2) in
        match op with
        | Plus -> binary "+"
        | Minus -> binary "-"
        | Times -> binary "*"
        | Int_Divide -> binary "%/%"
        | Modulo -> binary "%%"
        | Less -> binary "<"
        | Less_Equal -> binary "<="
        | Greater -> binary ">"
        | Greater_Equal -> binary ">="
        | Equal -> binary "=="
        | Not_Equal -> binary "!="
        | Logical_And -> binary "&&"
        | Logical_Or -> binary "||" )
    | Subset1 (se1, None) -> Printf.sprintf "%s[]" (simple_expr_to_r se1)
    | Subset1 (se1, Some se2) ->
        Printf.sprintf "%s[%s]" (simple_expr_to_r se1) (simple_expr_to_r se2)
    | Subset2 (se1, se2) -> Printf.sprintf "%s[[%s]]" (simple_expr_to_r se1) (simple_expr_to_r se2)
    | Call (f, args) ->
        let inner = args |> List.map simple_expr_to_r |> String.concat ", " in
        Printf.sprintf "%s(%s)" f inner
    | Simple_Expression se -> simple_expr_to_r se in

  let rec stmt_to_r ?(depth = 0) stmt =
    let block_to_r stmts =
      let padding depth = String.make (depth * 2) ' ' in
      let stmt_to_r' stmt =
        let depth = depth + 1 in
        padding depth ^ stmt_to_r ~depth stmt in
      let stmts' = stmts |> List.map stmt_to_r' |> String.concat "\n" in
      Printf.sprintf "{\n%s\n%s}" stmts' (padding depth) in
    match stmt with
    | Assign (x, e) -> Printf.sprintf "%s <- %s" x (expr_to_r e)
    | Subset1_Assign (x, None, e) -> Printf.sprintf "%s[] <- %s" x (expr_to_r e)
    | Subset1_Assign (x, Some se, e) ->
        Printf.sprintf "%s[%s] <- %s" x (simple_expr_to_r se) (expr_to_r e)
    | Subset2_Assign (x, se, e) ->
        Printf.sprintf "%s[[%s]] <- %s" x (simple_expr_to_r se) (expr_to_r e)
    | Function_Def (f, params, stmts) ->
        let params' = params |> String.concat ", " in
        Printf.sprintf "%s <- function (%s) %s" f params' (block_to_r stmts)
    | If (e, s1, []) -> Printf.sprintf "if (%s) %s" (expr_to_r e) (block_to_r s1)
    | If (e, s1, s2) ->
        Printf.sprintf "if (%s) %s else %s" (expr_to_r e) (block_to_r s1) (block_to_r s2)
    | For (x, e, s) -> Printf.sprintf "for (%s in %s) %s" x (expr_to_r e) (block_to_r s)
    | Expression e -> expr_to_r e in

  stmt_list |> List.map stmt_to_r |> String.concat "\n"

open Expr

include Set.Make (Identifier)

let over_list f xs = List.fold_left (fun set x -> union (f x) set) empty xs

let collect_se = function
  | Lit _ -> empty
  | Var x -> singleton x

let collect_e = function
  | Combine ses -> over_list collect_se ses
  | Dataframe_Ctor _ -> raise Common.Not_supported
  | Unary_Op (_, se) -> collect_se se
  | Binary_Op (_, se1, se2) -> union (collect_se se1) (collect_se se2)
  | Subset1 (se1, None) -> collect_se se1
  | Subset1 (se1, Some se2) -> union (collect_se se1) (collect_se se2)
  | Subset2 (se1, se2) -> union (collect_se se1) (collect_se se2)
  | Call (f, args) -> union (singleton f) (over_list collect_se args)
  | Simple_Expression se -> collect_se se

let rec collect_stmt stmt =
  match stmt with
  | Assign (x, e) -> union (singleton x) (collect_e e)
  | Subset1_Assign (x1, None, se3) -> union (singleton x1) (collect_se se3)
  | Subset1_Assign (x1, Some se2, se3) ->
      singleton x1 |> union (collect_se se2) |> union (collect_se se3)
  | Subset2_Assign (x1, se2, se3) ->
      singleton x1 |> union (collect_se se2) |> union (collect_se se3)
  | Function_Def (f, params, stmts) ->
      singleton f |> union (over_list singleton params) |> union (over_list collect_stmt stmts)
  | If (se1, s2, s3) ->
      collect_se se1 |> union (over_list collect_stmt s2) |> union (over_list collect_stmt s3)
  | For (x1, se2, s3) -> singleton x1 |> union (collect_se se2) |> union (over_list collect_stmt s3)
  | Expression e -> collect_e e

open Containers
open Expr

class ['a] fold =
  object (self)
    method lit (acc : 'a) _ = acc
    method var (acc : 'a) _ = acc

    method combine (acc : 'a) ses = List.fold_left self#simple_expr acc ses

    method unary_op (acc : 'a) _ se = self#simple_expr acc se

    method binary_op (acc : 'a) _ se1 se2 =
      let acc1 = self#simple_expr acc se1 in
      self#simple_expr acc1 se2

    method subset1 (acc : 'a) se1 opt_se2 =
      let acc1 = self#simple_expr acc se1 in
      Option.fold self#simple_expr acc1 opt_se2

    method subset2 (acc : 'a) se1 se2 =
      let acc1 = self#simple_expr acc se1 in
      self#simple_expr acc1 se2

    method call (acc : 'a) _ ses = List.fold_left self#simple_expr acc ses

    method assign (acc : 'a) _ e = self#expr acc e

    method subset1_assign (acc : 'a) _ opt_se1 se2 =
      let acc1 = Option.fold self#simple_expr acc opt_se1 in
      self#simple_expr acc1 se2

    method subset2_assign (acc : 'a) _ se1 se2 =
      let acc1 = self#simple_expr acc se1 in
      self#simple_expr acc1 se2

    method if_stmt (acc : 'a) se1 stmts2 stmts3 =
      let acc1 = self#simple_expr acc se1 in
      let acc2 = List.fold_left self#stmt acc1 stmts2 in
      List.fold_left self#stmt acc2 stmts3

    method for_stmt (acc : 'a) _ se2 stmts3 =
      let acc1 = self#simple_expr acc se2 in
      List.fold_left self#stmt acc1 stmts3

    method function_def (acc : 'a) _ _ stmts3 = List.fold_left self#stmt acc stmts3

    method print (acc : 'a) e = self#expr acc e

    method simple_expr (acc : 'a) =
      function
      | Lit l -> self#lit acc l
      | Var x -> self#var acc x

    method expr (acc : 'a) =
      function
      | Combine ses -> self#combine acc ses
      | Dataframe_Ctor _ -> raise Not_supported
      | Unary_Op (op, se) -> self#unary_op acc op se
      | Binary_Op (op, se1, se2) -> self#binary_op acc op se1 se2
      | Subset1 (se1, opt_se2) -> self#subset1 acc se1 opt_se2
      | Subset2 (se1, se2) -> self#subset2 acc se1 se2
      | Call (f, args) -> self#call acc f args
      | Simple_Expression se -> self#simple_expr acc se

    method stmt (acc : 'a) =
      function
      | Assign (x, e) -> self#assign acc x e
      | Subset1_Assign (x, se1, opt_se2) -> self#subset1_assign acc x se1 opt_se2
      | Subset2_Assign (x, se1, se2) -> self#subset2_assign acc x se1 se2
      | Function_Def (f, params, stmts) -> self#function_def acc f params stmts
      | If (se1, stmts2, stmts3) -> self#if_stmt acc se1 stmts2 stmts3
      | For (x1, se2, stmts3) -> self#for_stmt acc x1 se2 stmts3
      | Print e -> self#print acc e
      | Expression e -> self#expr acc e
  end

let literals =
  object
    inherit [literal list] fold
    method! lit acc l = l :: acc
  end

let variables =
  object
    inherit [identifier list] fold as super
    method! var acc x = x :: acc
    method! assign acc x e = super#assign (x :: acc) x e
    method! subset1_assign acc x opt_se1 se2 = super#subset1_assign (x :: acc) x opt_se1 se2
    method! subset2_assign acc x se1 se2 = super#subset2_assign (x :: acc) x se1 se2
    method! for_stmt acc x1 se2 stmts3 = super#for_stmt (x1 :: acc) x1 se2 stmts3
  end

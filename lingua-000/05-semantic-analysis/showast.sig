signature SHOW_AST =
sig
  val showOper : Ast.oper -> string
  val showVar : Ast.var -> string
  val showExp : Ast.exp -> string
  val showDec : Ast.dec -> string
  val showTy : Ast.ty -> string
  val showField : Ast.field -> string
  val showFundec : Ast.fundec -> string
end

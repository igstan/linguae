signature FREE_VARS =
sig
  val freeVarsInVar : Ast.var -> unit Symbol.table
  val freeVarsInExp : Ast.exp -> unit Symbol.table
  val freeVarsInDec : Ast.dec -> unit Symbol.table
end

signature FREE_VAR_ANALYSIS =
sig
  val analyseVar : Ast.var -> unit Symbol.table
  val analyseExp : Ast.exp -> unit Symbol.table
  val analyseDec : Ast.dec -> unit Symbol.table
end

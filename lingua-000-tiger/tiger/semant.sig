signature SEMANT =
sig
  val translateProgram : Ast.exp -> Translate.frag list
end

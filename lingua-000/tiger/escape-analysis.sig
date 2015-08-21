(**
 * Analyses the AST for identifiers accessed in nested functions and marks
 * them as such using the mutable `escape` field.
 *)
signature ESCAPE_ANALYSIS =
sig
  val analyse : Ast.exp -> unit
end

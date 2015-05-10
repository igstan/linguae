package ro.igstan.debugger
package typer

import syntax.Term

object Typer {
  def typeOf(term: Term, tenv: TypeEnv): Either[String, TypeScheme] = {
    val ty = Type.Var.fresh()
    val constraints = constrain(term, tenv, ty)

    unify(constraints) match {
      case Left(error) => Left(error)
      case Right(substitution) => Right(tenv.generalize(substitution.apply(ty)))
    }
  }

  def constrain(term: Term, tenv: TypeEnv, expectedTy: Type): List[Constraint] = {
    ???
  }

  def unify(constraints: List[Constraint]): Either[String, Substitution] = {
    ???
  }
}

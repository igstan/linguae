package ro.igstan.debugger
package typer

sealed trait Constraint
object Constraint {
  case class EQ(a: Type, b: Type) extends Constraint
  case class GEN(tenv: TypeEnv, tyScheme: TypeScheme, ty: Type) extends Constraint
  case class INST(tsvar: TypeScheme.Var, ty: Type) extends Constraint
}

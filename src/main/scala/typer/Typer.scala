package ro.igstan.debugger
package typer

import syntax.Term, Term._

object Typer {
  def typeOf(term: Term, tenv: TypeEnv): Either[String, TypeScheme] = {
    val ty = Type.Var.fresh()

    for {
      constraints <- constrain(term, tenv, ty).right
      substitution <- unify(constraints).right
    } yield {
      tenv.generalize(substitution.apply(ty))
    }
  }

  def constrain(term: Term, tenv: TypeEnv, expectedTy: Type): Either[String, List[Constraint]] = {
    term match {
      case INT(value) => Right(List(Constraint.EQ(expectedTy, TINT)))
      case BOOL(value) => Right(List(Constraint.EQ(expectedTy, TBOOL)))
      case ADD(a, b) =>
        for {
          constraintsA <- constrain(a, tenv, TINT).right
          constraintsB <- constrain(b, tenv, TINT).right
        } yield {
          Constraint.EQ(expectedTy, TINT) :: constraintsA ++ constraintsB
        }
      case SUB(a, b) =>
        for {
          constraintsA <- constrain(a, tenv, TINT).right
          constraintsB <- constrain(b, tenv, TINT).right
        } yield {
          Constraint.EQ(expectedTy, TINT) :: constraintsA ++ constraintsB
        }
      case VAR(name) =>
        tenv.get(name) match {
          case None =>
            Left(s"unbound identifier: $name")
          case Some(TypeScheme.FORALL(_, ty)) =>
            Right(List(Constraint.EQ(expectedTy, ty)))
          case Some(TypeScheme.TSVAR(tsvar)) =>
            Right(List(Constraint.INST(tsvar, expectedTy)))
        }
      case IF(test, yes, no) =>
        val yesTy = Type.Var.fresh()
        val noTy = Type.Var.fresh()
        for {
          ifConstr <- constrain(test, tenv, TBOOL).right
          yesConstr <- constrain(yes, tenv, yesTy).right
          noConstr <- constrain(no, tenv, noTy).right
        } yield {
          List(
            Constraint.EQ(expectedTy, yesTy),
            Constraint.EQ(yesTy, noTy)
          ) ++ ifConstr ++ yesConstr ++ noConstr
        }
      case FN(param, body) =>
        val paramTy = Type.Var.fresh()
        val bodyTy = Type.Var.fresh()
        val extendedTenv = tenv.set(param, TypeScheme.FORALL(Set.empty, paramTy))

        for {
          bodyConstr <- constrain(body, extendedTenv, bodyTy).right
        } yield {
          Constraint.EQ(expectedTy, TFUN(paramTy, bodyTy)) :: bodyConstr
        }
      case APP(fn, arg) =>
        val argTy = Type.Var.fresh()

        for {
          fnConstr <- constrain(fn, tenv, TFUN(argTy, expectedTy)).right
          argConstr <- constrain(arg, tenv, argTy).right
        } yield {
          fnConstr ++ argConstr
        }
      case LET(binding, value, body) =>
        val bindingScheme = TypeScheme.Var.fresh()
        val valueTy = Type.Var.fresh()
        val extendedTenv = tenv.set(binding, bindingScheme)

        for {
          valueConstr <- constrain(value, tenv, valueTy).right
          bodyConstr <- constrain(body, extendedTenv, expectedTy).right
        } yield {
          valueConstr ++ List(Constraint.GEN(tenv, bindingScheme, valueTy)) ++ bodyConstr
        }
    }
  }

  def unify(constraints: List[Constraint]): Either[String, Substitution] = {
    ???
  }
}

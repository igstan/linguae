package ro.igstan.debugger
package typer

import syntax.Term, Term._

object Typer {
  def typeOf(term: Term, tenv: TypeEnv): Either[String, TypeScheme] = {
    val ty = Type.Var.fresh()

    for {
      constraints <- constrain(term, tenv, ty)
      substitution <- unify(constraints)
    } yield {
      tenv.generalize(substitution.apply(ty))
    }
  }

  def constrain(term: Term, tenv: TypeEnv, expectedTy: Type): Either[String, List[Constraint]] = {
    term match {
      case INT(_) => Right(List(Constraint.EQ(expectedTy, TINT)))
      case BOOL(_) => Right(List(Constraint.EQ(expectedTy, TBOOL)))
      case ADD(a, b) =>
        for {
          constraintsA <- constrain(a, tenv, TINT)
          constraintsB <- constrain(b, tenv, TINT)
        } yield {
          Constraint.EQ(expectedTy, TINT) :: constraintsA ++ constraintsB
        }
      case SUB(a, b) =>
        for {
          constraintsA <- constrain(a, tenv, TINT)
          constraintsB <- constrain(b, tenv, TINT)
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
          ifConstr <- constrain(test, tenv, TBOOL)
          yesConstr <- constrain(yes, tenv, yesTy)
          noConstr <- constrain(no, tenv, noTy)
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
          bodyConstr <- constrain(body, extendedTenv, bodyTy)
        } yield {
          Constraint.EQ(expectedTy, TFUN(paramTy, bodyTy)) :: bodyConstr
        }
      case APP(fn, arg) =>
        val argTy = Type.Var.fresh()

        for {
          fnConstr <- constrain(fn, tenv, TFUN(argTy, expectedTy))
          argConstr <- constrain(arg, tenv, argTy)
        } yield {
          fnConstr ++ argConstr
        }
      case LET(binding, value, body) =>
        val bindingScheme = TypeScheme.Var.fresh()
        val valueTy = Type.Var.fresh()
        val extendedTenv = tenv.set(binding, bindingScheme)

        for {
          valueConstr <- constrain(value, tenv, valueTy)
          bodyConstr <- constrain(body, extendedTenv, expectedTy)
        } yield {
          valueConstr ++ List(Constraint.GEN(tenv, bindingScheme, valueTy)) ++ bodyConstr
        }
    }
  }

  def unify1(constraints: List[Constraint]): Either[String, Substitution] = {
    import Constraint._

    @annotation.tailrec
    def loop(constraints: List[Constraint], subst: Either[String, Substitution]): Either[String, Substitution] = {
      constraints match {
        case Nil => subst
        case constr :: rest =>
          constr match {
            case EQ(TINT, TINT) => loop(rest, subst)
            case EQ(TBOOL, TBOOL) => loop(rest, subst)
            case EQ(TVAR(a), TVAR(b)) if a == b => loop(rest, subst)
            case EQ(TVAR(tvar), ty @ TVAR(_)) =>
              subst match {
                case Left(error) => Left(error)
                case Right(subst) =>
                  loop(
                    substituteConstraints(Substitution.one(tvar, ty), rest),
                    Right(subst.compose(Substitution.one(tvar, ty)))
                  )
              }
            case EQ(TVAR(tvar), ty) =>
              if (tvar.occursIn(ty)) {
                Left(s"occurs check: $tvar in $ty")
              } else {
                subst match {
                  case Left(error) => Left(error)
                  case Right(subst) =>
                    loop(
                      substituteConstraints(Substitution.one(tvar, ty), rest),
                      Right(subst.compose(Substitution.one(tvar, ty)))
                    )
                }
              }
            case EQ(ty, TVAR(tvar)) => unify1(EQ(TVAR(tvar), ty) :: rest)
            case EQ(TFUN(p1, r1), TFUN(p2, r2)) => unify1(EQ(p1, p2) :: EQ(r1, r2) :: rest)
            case EQ(a, b) => Left(s"can't unify $a with $b")
            case a: GEN => sys.error(s"bug: GEN constraint in unify1 $a")
            case a: INST => sys.error(s"bug: GEN constraint in unify1 $a")
          }
      }
    }

    loop(constraints, Right(Substitution.empty))
  }

  def instantiateAll(constraints: List[Constraint], tenv: TypeEnv, ty: Type): List[Constraint] = {
    ???
  }

  def unify2(constraints: List[Constraint]): Either[String, Substitution] = {
    import Constraint._

    def loop(constraints: List[Constraint], subst: Either[String, Substitution]): Either[String, Substitution] = {
      constraints match {
        case Nil => subst
        case constr :: rest =>
          constr match {
            case GEN(tenv, tyScheme, ty) =>
              val (instConstr, tail) = rest.partition {
                case INST(tsVar, _) =>
                  tyScheme match {
                    case _: TypeScheme.FORALL => false
                    case TypeScheme.TSVAR(v) => tsVar == v
                  }
                case _ => false
              }

              val instances = instantiateAll(instConstr, tenv, ty)

              unify1(instances) match {
                case Left(error) => Left(error)
                case Right(s1) =>
                  val constr = substituteConstraints(s1, tail)
                  unify2(constr) match {
                    case Left(error) => Left(error)
                    case Right(s2) => Right(s1.compose(s2))
                  }
              }
            case _ => sys.error(s"bug: INST constraint seen before GEN constraint")
          }
      }
    }

    loop(constraints, Right(Substitution.empty))
  }

  def substTenv(subst: Substitution, tenv: TypeEnv): TypeEnv = {
    TypeEnv {
      tenv.bindings.map {
        case (k, TypeScheme.FORALL(tvars, ty)) => k -> TypeScheme.FORALL(tvars, subst.apply(ty))
        case (k, tyScheme) => k -> tyScheme
      }
    }
  }

  def substConstraint(subst: Substitution)(constraint: Constraint): Constraint = {
    import Constraint._
    constraint match {
      case EQ(a, b) => EQ(subst.apply(a), subst.apply(b))
      case GEN(tenv, tyScheme, ty) => GEN(substTenv(subst, tenv), tyScheme, subst.apply(ty))
      case INST(tyScheme, ty) => INST(tyScheme, subst.apply(ty))
    }
  }

  def substituteConstraints(subst: Substitution, constraints: List[Constraint]): List[Constraint] = {
    constraints.map(substConstraint(subst))
  }

  def unify(constraints: List[Constraint]): Either[String, Substitution] = {
    val (eqConstraints, otherConstraints) = constraints.partition {
      case _: Constraint.EQ => true
      case _ => false
    }

    for {
      s1 <- unify1(eqConstraints)
      s2 <- unify2(substituteConstraints(s1, otherConstraints))
    } yield {
      s1.compose(s2)
    }
  }
}

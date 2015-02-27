structure Unify =
struct
  structure TypedTerm =
  struct
    datatype ty =
      VAR of Type.ty * Term.Var.ty
    | FUN of Type.ty * Term.Var.ty * ty
    | APP of Type.ty * ty * ty
  end

  fun annotate (term : Term.ty) : TypedTerm.ty =
    let
      fun loop term tenv =
        case term of
          Term.VAR (var) =>
          let in
            case TEnv.get tenv var of
              NONE    => TypedTerm.VAR (Type.freshVar (), var)
            | SOME ty => TypedTerm.VAR (ty, var)
          end
        | Term.FUN (param, body) =>
          let
            val tenv' = TEnv.set tenv param (Type.freshVar ())
          in
            TypedTerm.FUN (Type.freshVar (), param, loop body tenv')
          end
        | Term.APP (def, arg) =>
            TypedTerm.APP (Type.freshVar (), loop def tenv, loop arg tenv)
    in
      Type.resetFreshness();
      loop term TEnv.empty
    end
end

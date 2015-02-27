structure Unify =
struct
  structure TypedTerm =
  struct
    datatype ty =
      VAR of Type.ty * Term.Var.ty
    | FUN of Type.ty * Term.Var.ty * ty
    | APP of Type.ty * ty * ty

    fun typeOf (VAR t) = #1 t
      | typeOf (FUN t) = #1 t
      | typeOf (APP t) = #1 t
  end

  type constraint = Type.ty * Type.ty

  fun annotate (term : Term.ty) : TypedTerm.ty =
    let
      fun loop term tenv =
        case term of
          Term.VAR (var) =>
          let in
            case TEnv.get tenv var of
              NONE => raise Fail ("unbound identifier: " ^ String.toString var)
            | SOME ty => TypedTerm.VAR (ty, var)
          end
        | Term.FUN (param, body) =>
          let
            val paramTy = Type.freshVar ()
            val tenv' = TEnv.set tenv param paramTy
            val annotatedBody = loop body tenv'
            val bodyTy = TypedTerm.typeOf annotatedBody
            val funTy = Type.FUN (paramTy, bodyTy)
          in
            TypedTerm.FUN (funTy, param, annotatedBody)
          end
        | Term.APP (def, arg) =>
            TypedTerm.APP (Type.freshVar (), loop def tenv, loop arg tenv)
    in
      Type.resetFreshness();
      loop term TEnv.empty
    end

  fun constrain (term : TypedTerm.ty) : constraint list =
    let
      fun loop [] constraints = constraints
        | loop (term :: terms) constraints =
          case term of
            TypedTerm.VAR _ => loop terms constraints
          | TypedTerm.FUN (_, _, body) => loop (body :: terms) constraints
          | TypedTerm.APP (returnTy, def, arg) =>
            let
              val defTy = TypedTerm.typeOf def
              val argTy = TypedTerm.typeOf arg
              val appC = (defTy, Type.FUN (argTy, returnTy))
            in
              loop (def :: arg :: terms) (appC :: constraints)
            end
    in
      loop [term] []
    end

  local
    fun occurs v ty =
      case ty of
        Type.VAR v' => v = v'
      | Type.FUN (param, return) => occurs v param orelse occurs v return

    fun unifyVar v (ty as Type.VAR v') =
          if v = v' then
            Subst.empty
          else
            Subst.fromList [(v, ty)]
      | unifyVar v ty =
          if occurs v ty then
            raise Fail "occurs check"
          else
            Subst.fromList [(v, ty)]

    fun unifyPair pair =
      case pair of
        (Type.VAR (v), ty) => unifyVar v ty
      | (ty, Type.VAR (v)) => unifyVar v ty
      | (Type.FUN (param1, return1), Type.FUN (param2, return2)) =>
          unifyMany [(param1, param2), (return1, return2)]

    and unifyMany [] = Subst.empty
      | unifyMany ((t1, t2) :: constraints) =
        let
          val s1 = unifyMany constraints
          val s2 = unifyPair (Subst.apply s1 t1, Subst.apply s1 t2)
        in
          Subst.compose s1 s2
        end
  in
    fun unify constraints = unifyMany constraints
  end
end

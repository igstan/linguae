structure Unify =
struct
  structure TypedTerm =
  struct
    datatype ty =
      INT of Type.ty
    | BOOL of Type.ty
    | VAR of Type.ty * Term.Var.ty
    | IF of Type.ty * ty * ty * ty
    | FUN of Type.ty * Term.Var.ty * ty
    | APP of Type.ty * ty * ty
    | LET of Type.ty * Term.Var.ty * ty * ty

    fun typeOf (INT t) = t
      | typeOf (BOOL t) = t
      | typeOf (VAR t) = #1 t
      | typeOf (IF t) = #1 t
      | typeOf (FUN t) = #1 t
      | typeOf (APP t) = #1 t
      | typeOf (LET t) = #1 t
  end

  exception UnificationFailure of Type.ty * Type.ty
  exception OccursCheck of Type.Var.ty * Type.ty

  type constraint = Type.ty * Type.ty

  fun annotate term tenv =
    case term of
      Term.VAR (var) =>
      let in
        case TEnv.get tenv var of
          NONE => raise Fail ("unbound identifier: " ^ String.toString var)
        | SOME ty => TypedTerm.VAR (ty, var)
      end
    | Term.INT _ => TypedTerm.INT Type.INT
    | Term.BOOL _ => TypedTerm.BOOL Type.BOOL
    | Term.IF (test, yes, no) =>
      let
        val testTy = annotate test tenv
        val thenTy = annotate yes tenv
        val elseTy = annotate no tenv
      in
        TypedTerm.IF (Type.freshVar (), testTy, thenTy, elseTy)
      end
    | Term.FUN (param, body) =>
      let
        val paramTy = Type.freshVar ()
        val tenv' = TEnv.set tenv param paramTy
        val annotatedBody = annotate body tenv'
        val bodyTy = TypedTerm.typeOf annotatedBody
        val funTy = Type.FUN (paramTy, bodyTy)
      in
        TypedTerm.FUN (funTy, param, annotatedBody)
      end
    | Term.APP (def, arg) =>
        TypedTerm.APP (Type.freshVar (), annotate def tenv, annotate arg tenv)
    | Term.LET (var, value, body) =>
      let
        val annValue = annotate value tenv
        val tenv' = TEnv.set tenv var (TypedTerm.typeOf annValue)
        val annBody = annotate body tenv'
      in
        TypedTerm.LET (Type.freshVar (), var, annValue, annBody)
      end

  fun constrain (term : TypedTerm.ty) : constraint list =
    let
      fun loop [] constraints = constraints
        | loop (term :: terms) constraints =
          case term of
            TypedTerm.INT _ => loop terms constraints
          | TypedTerm.BOOL _ => loop terms constraints
          | TypedTerm.VAR _ => loop terms constraints
          | TypedTerm.IF (ifTy, test, yes, no) =>
            let
              val ifC = (ifTy, TypedTerm.typeOf yes)
              val testC = (TypedTerm.typeOf test, Type.BOOL)
              val branchC = (TypedTerm.typeOf yes, TypedTerm.typeOf no)
            in
              loop (test :: yes :: no :: terms) (ifC :: testC :: branchC :: constraints)
            end
          | TypedTerm.FUN (_, _, body) => loop (body :: terms) constraints
          | TypedTerm.APP (returnTy, def, arg) =>
            let
              val defTy = TypedTerm.typeOf def
              val argTy = TypedTerm.typeOf arg
              val appC = (defTy, Type.FUN (argTy, returnTy))
            in
              loop (def :: arg :: terms) (appC :: constraints)
            end
          | TypedTerm.LET (letTy, _, value, body) =>
            let
              val letC = (letTy, TypedTerm.typeOf body)
            in
              loop (value :: body :: terms) (letC :: constraints)
            end
    in
      loop [term] []
    end

  local
    fun occurs v ty =
      case ty of
        Type.INT => false
      | Type.BOOL => false
      | Type.VAR v' => v = v'
      | Type.FUN (param, return) => occurs v param orelse occurs v return

    fun unifyVar v (ty as Type.VAR v') =
          if v = v' then
            Subst.empty
          else
            Subst.fromList [(v, ty)]
      | unifyVar v ty =
          if occurs v ty then
            raise OccursCheck (v, ty)
          else
            Subst.fromList [(v, ty)]

    fun unifyPair pair =
      case pair of
        (Type.INT, Type.INT) => Subst.empty
      | (Type.BOOL, Type.BOOL) => Subst.empty
      | (Type.VAR (v), ty) => unifyVar v ty
      | (ty, Type.VAR (v)) => unifyVar v ty
      | (Type.FUN (param1, return1), Type.FUN (param2, return2)) =>
          unifyMany [(param1, param2), (return1, return2)]
      | _ => raise UnificationFailure pair

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

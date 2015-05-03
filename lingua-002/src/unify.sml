structure Unify =
struct
  exception UnificationFailure of Type.ty * Type.ty
  exception OccursCheck of Type.Var.ty * Type.ty

  datatype constraint =
    EQ of Type.ty * Type.ty

  (**
   * Walks over a typed tree and records constraints along the way. Produces a
   * list of constraints amenable to unification.
   *)
  fun constrain tenv ty term =
    case term of
      Term.INT _ => []
    | Term.BOOL _ => []
    | Term.VAR var =>
      let in
        case TypeEnv.get tenv var of
          NONE => raise Fail ("unbound identifier: " ^ String.toString var)
        | SOME (TypeScheme.ForAll (_, varTy)) => [EQ (ty, varTy)]
      end
    | Term.FUN (param, body) =>
      let
        val paramTy = Type.freshVar ()
        val bodyTy = Type.freshVar ()
        val tenv' = TypeEnv.set tenv param (TypeScheme.ForAll ([], paramTy))
        val bodyC = constrain tenv' bodyTy body
      in
        (EQ (ty, Type.FUN (paramTy, bodyTy))) :: bodyC
      end
    | Term.IF (test, yes, no) =>
      let
        val testTy = Type.freshVar ()
        val yesTy = Type.freshVar ()
        val noTy = Type.freshVar ()
        val ifC = EQ (yesTy, ty)
        val testC = constrain tenv Type.BOOL test
        val yesC = constrain tenv noTy yes
        val noC = constrain tenv yesTy no
      in
        ifC :: testC @ yesC @ noC
      end
    | Term.APP (def, arg) =>
      let
        val argTy = Type.freshVar ()
        val defC = constrain tenv (Type.FUN (argTy, ty)) def
        val argC = constrain tenv argTy arg
      in
        defC @ argC
      end
    | Term.LET (binding, value, body) =>
      let
        val bindingTy = Type.freshVar ()
        val valueTy = Type.freshVar ()

        val valueC = constrain tenv valueTy value
        val tenv' = TypeEnv.set tenv binding (TypeScheme.ForAll ([], bindingTy))
        val bodyC = constrain tenv' ty body
        val bindingC = EQ (bindingTy, valueTy)
      in
        bindingC :: valueC @ bodyC
      end

  local
    fun occurs v ty =
      case ty of
        Type.INT => false
      | Type.BOOL => false
      | Type.VAR v' => v = v'
      | Type.FUN (param, return) => occurs v param orelse occurs v return

    (**
     * Unifies two type variables.
     *)
    fun unifyVar v (ty as Type.VAR v') =
        if v = v'
        then Subst.empty
        else Subst.fromList [(v, ty)]
      | unifyVar v ty =
        if occurs v ty
        then raise OccursCheck (v, ty)
        else Subst.fromList [(v, ty)]

    (**
     * Unifies a single constraint pair.
     *)
    fun unifyPair pair =
      case pair of
        (Type.INT, Type.INT) => Subst.empty
      | (Type.BOOL, Type.BOOL) => Subst.empty
      | (Type.VAR (v), ty) => unifyVar v ty
      | (ty, Type.VAR (v)) => unifyVar v ty
      | (Type.FUN (param1, return1), Type.FUN (param2, return2)) =>
          unifyMany [EQ (param1, param2), EQ (return1, return2)]
      | _ => raise UnificationFailure pair

    (**
     * Unifies a set of constraint pairs.
     *)
    and unifyMany [] = Subst.empty
      | unifyMany (EQ (t1, t2) :: constraints) =
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

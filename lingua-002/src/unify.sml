structure Unify =
struct
  open Constraint

  exception UnificationFailure of Type.ty * Type.ty
  exception OccursCheck of Type.Var.ty * Type.ty

  (**
   * Walks over a typed tree and records constraints along the way. Produces a
   * list of constraints amenable to unification.
   *)
  fun constrain tenv ty term =
    case term of
      Term.INT _ => [EQ (ty, Type.INT)]
    | Term.BOOL _ => [EQ (ty, Type.BOOL)]
    | Term.VAR var =>
      let in
        case TypeEnv.get tenv var of
          NONE => raise Fail ("unbound identifier: " ^ String.toString var)
        | SOME (TypeScheme.FORALL (_, ty')) => [EQ (ty, ty')]
        | SOME (TypeScheme.SVAR var) => [INST (var, ty)]
      end
    | Term.FUN (param, body) =>
      let
        val paramTy = Type.freshVar ()
        val bodyTy = Type.freshVar ()
        val tenv' = TypeEnv.set tenv param (TypeScheme.FORALL ([], paramTy))
        val bodyC = constrain tenv' bodyTy body
      in
        (EQ (ty, Type.FUN (paramTy, bodyTy))) :: bodyC
      end
    | Term.IF (test, yes, no) =>
      let
        val yesTy = Type.freshVar ()
        val noTy = Type.freshVar ()
        val ifC = EQ (yesTy, ty)
        val branchC = EQ (yesTy, noTy)
        val testC = constrain tenv Type.BOOL test
        val yesC = constrain tenv yesTy yes
        val noC = constrain tenv noTy no
      in
        [ifC, branchC] @ testC @ yesC @ noC
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
        val bindingScheme = TypeScheme.freshVar ()
        val valueTy = Type.freshVar ()

        val valueC = constrain tenv valueTy value
        val tenv' = TypeEnv.set tenv binding bindingScheme
        val bodyC = constrain tenv' ty body
        val bindingC = GEN (tenv, bindingScheme, valueTy)
      in
        valueC @ [bindingC] @ bodyC
      end

  local
    fun occurs v ty =
      case ty of
        Type.INT => false
      | Type.BOOL => false
      | Type.VAR v' => v = v'
      | Type.FUN (param, return) => occurs v param orelse occurs v return

    fun substConstraint subst constraint =
      case constraint of
        EQ (tyA, tyB) =>
          EQ (Subst.apply subst tyA, Subst.apply subst tyB)
      | GEN (tenv, typeScheme, ty) =>
          GEN (TypeEnv.substitute tenv subst, typeScheme, Subst.apply subst ty)
      | INST (typeScheme, ty) =>
          INST (typeScheme, Subst.apply subst ty)

    fun substConstraints subst constraints =
      List.map (substConstraint subst) constraints

    fun unify1 [] = Subst.empty
      | unify1 (head :: tail) =
        case head of
          EQ (Type.INT, Type.INT) => unify1 tail
        | EQ (Type.BOOL, Type.BOOL) => unify1 tail
        | EQ (Type.VAR a, ty as Type.VAR b) =>
            if a = b
            then unify1 tail
            else Subst.compose (Subst.fromList [(a, ty)]) (unify1 (substConstraints (Subst.fromList [(a, ty)]) tail))
        | EQ (Type.VAR a, ty) =>
            if occurs a ty
            then raise OccursCheck (a, ty)
            else Subst.compose (Subst.fromList [(a, ty)]) (unify1 (substConstraints (Subst.fromList [(a, ty)]) tail))
        | EQ (ty, Type.VAR a) => unify1 ((EQ (Type.VAR a, ty)) :: tail)
        | EQ (Type.FUN (paramA, returnA), Type.FUN (paramB, returnB)) =>
            unify1 ((EQ (paramA, paramB)) :: (EQ (returnA, returnB)) :: tail)
        | EQ pair => raise UnificationFailure pair
        | GEN _ => raise Fail "Bug: GEN constraint encountered."
        | INST _ => raise Fail "Bug: INST constraint encountered."

    fun instantiateAll insts tenv ty =
      let
        val gen = TypeEnv.generalize tenv ty
        fun loop insts =
          case insts of
            [] => []
          | INST (_, ty) :: tail =>
            let
              val instTy = TypeScheme.instantiate gen
              val constrs = loop tail
            in
              EQ (ty, instTy) :: constrs
            end
          | _ => raise Fail "Bug: bad constraint type in instantiateAll."
      in
        loop insts
      end

    fun isEQ (EQ _) = true
      | isEQ _ = false

    fun isINST (TypeScheme.FORALL _) _ = false
      | isINST (TypeScheme.SVAR typeScheme) constraint =
        case constraint of
          INST (typeScheme', ty) => typeScheme = typeScheme'
        | _ => false

    fun unify2 [] = Subst.empty
      | unify2 (GEN (tenv, typeScheme, ty) :: tail) =
        let
          val (instConstraints, rest) = List.partition (isINST typeScheme) tail
          val insts = instantiateAll instConstraints tenv ty
          val s1 = unify1 insts
          val c' = substConstraints s1 rest
        in
          Subst.compose s1 (unify2 c')
        end
      | unify2 _ = raise Fail "Bug: INST constraint seen before GEN constraint."
  in
    fun unify constraints =
      let
        val (eqConstraints, otherConstraints) = List.partition isEQ constraints
        val s1 = unify1 eqConstraints
        val s2 = unify2 (substConstraints s1 otherConstraints)
        val s = Subst.compose s1 s2
      in
        s
      end
  end
end

structure Constraint =
struct
  datatype ty =
    EQ of Type.ty * Type.ty
  | GEN of TypeEnv.ty * TypeScheme.ty * Type.ty
  | INST of TypeScheme.Var.ty * Type.ty

  fun isEQ (EQ _) = true
    | isEQ _ = false

  fun isINST (TypeScheme.SVAR tsVarA) (INST (tsVarB, _)) = tsVarA = tsVarB
    | isINST _ _ = false

  fun substitute constraints subst =
    let
      fun substitute constraint =
        case constraint of
          EQ (tyA, tyB) =>
          EQ (Subst.apply subst tyA, Subst.apply subst tyB)
        | GEN (tenv, typeScheme, ty) =>
          GEN (TypeEnv.substitute tenv subst, typeScheme, Subst.apply subst ty)
        | INST (typeScheme, ty) =>
          INST (typeScheme, Subst.apply subst ty)
    in
      List.map substitute constraints
    end

  (**
   * Instantiate a list of INST constraints based on a type environment.
   *)
  fun instantiate constraints tenv ty =
    let
      val gen = TypeEnv.generalize tenv ty
      fun loop constraints =
        case constraints of
          [] => []
        | INST (_, ty) :: tail =>
          let
            val instTy = TypeScheme.instantiate gen
            val constrs = loop tail
          in
            EQ (ty, instTy) :: constrs
          end
        | _ => raise Fail "Bug: bad constraint type in `instantiate`."
    in
      loop constraints
    end

  fun toString constraint =
    let
      open Show
    in
      case constraint of
        EQ (a, b) => "EQ " ^ (tuple2 (Type.toString, Type.toString) (a, b))
      | GEN (_, b, c) => "GEN " ^ (tuple2 (TypeScheme.toString, Type.toString) (b, c))
      | INST (a, b) => "INST " ^ (tuple2 (Int.toString, Type.toString) (a, b))
    end
end

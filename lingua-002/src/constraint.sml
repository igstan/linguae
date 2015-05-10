structure Constraint =
struct
  datatype ty =
    EQ of Type.ty * Type.ty
  | GEN of TypeEnv.ty * TypeScheme.ty * Type.ty
  | INST of TypeScheme.Var.ty * Type.ty

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

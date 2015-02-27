structure TypeEnv = TypeEnvFn (
  structure TermMap = BinaryMapFn (Term.Var.Key)
  structure TypeSet = BinarySetFn (Type.Var.Key)
)

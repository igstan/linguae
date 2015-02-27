structure TEnv = TEnvFn (
  structure TermMap = BinaryMapFn (Term.Var.Key)
)

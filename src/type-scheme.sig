signature TYPE_SCHEME =
sig
  type ty

  (**
   * A type-scheme constructor.
   *)
  val forall : Type.Var.ty list -> Type.ty -> ty

  (**
   * Instantiate a type-scheme to a type by replacing bound type variables
   * with fresh ones.
   *)
  val instantiate : ty -> Type.ty

  (**
   * Gather this type-scheme's free type variables.
   *)
  val freeVars : ty -> Type.Var.ty list

  val toString : ty -> string
end

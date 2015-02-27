signature TYPE_ENV =
sig
  type ty

  val empty : ty
  val fromList : (Term.Var.ty * TypeScheme.ty) list -> ty
  val get : ty -> Term.Var.ty -> TypeScheme.ty option
  val set : ty -> Term.Var.ty -> TypeScheme.ty -> ty
  val freeVars : ty -> Type.Var.ty list
  val generalize : ty -> Type.ty -> TypeScheme.ty
end

signature SUBST =
sig
  type ty

  val empty : ty
  val fromList : (Type.Var.ty * Type.ty) list -> ty
  val apply : ty -> Type.ty -> Type.ty
  val compose : ty -> ty -> ty
end

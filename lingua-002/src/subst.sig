signature SUBST =
sig
  type ty

  val empty : ty
  val fromList : (Type.Var.ty * Type.ty) list -> ty
  val set : ty -> Type.Var.ty -> Type.ty -> ty
  val apply : ty -> Type.ty -> Type.ty
  val compose : ty -> ty -> ty
  val toString : ty -> string
end

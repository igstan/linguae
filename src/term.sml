structure Term =
struct
  structure Var =
  struct
    type ty = string
  end

  datatype ty =
    VAR of Var.ty
  | FUN of Var.ty * ty
  | APP of ty * ty
end

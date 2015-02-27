structure Type =
struct
  structure Var =
  struct
    type ty = int
  end

  datatype ty =
    VAR of Var.ty
  | FUN of ty * ty
end

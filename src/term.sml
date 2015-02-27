structure Term =
struct
  structure Var =
  struct
    type ty = string

    structure Key : ORD_KEY =
    struct
      type ord_key = ty
      val compare = String.compare
    end
  end

  datatype ty =
    VAR of Var.ty
  | FUN of Var.ty * ty
  | APP of ty * ty
end

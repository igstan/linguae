structure Type =
struct
  structure Var =
  struct
    type ty = int

    structure Key : ORD_KEY =
    struct
      type ord_key = ty
      val compare = Int.compare
    end
  end

  datatype ty =
    VAR of Var.ty
  | FUN of ty * ty
end

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

  local
    fun increment r = !r before r := !r + 1
    val counter = ref 0
  in
    fun freshVar () = VAR (increment counter)
    fun resetFreshness () = counter := 0
  end
end

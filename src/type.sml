structure Type =
struct
  structure Var =
  struct
    type ty = int
  end

  datatype ty =
    INT
  | VAR of Var.ty
  | FUN of ty * ty

  local
    val counter = ref 0
    fun increment r = !r before r := !r + 1
  in
    fun freshVar () = VAR (increment counter)
    fun resetFreshness () = counter := 0
  end

  fun toString INT = "int"
    | toString (VAR v) = "a" ^ Int.toString v
    | toString (FUN (p as FUN _, r)) = "(" ^ toString p ^ ") -> " ^ toString r
    | toString (FUN (p, r)) = toString p ^ " -> " ^ toString r
end

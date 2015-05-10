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
    INT
  | BOOL
  | VAR of Var.ty
  | FUN of ty * ty

  fun substitute ty tvar replacement =
    case ty of
      INT => ty
    | BOOL => ty
    | VAR candidate => if tvar = candidate then replacement else ty
    | FUN (paramTy, returnTy) =>
        FUN (
          substitute paramTy tvar replacement,
          substitute returnTy tvar replacement
        )

  (**
   * Obtain free type variables from within the given type.
   *)
  fun freeVars ty =
    case ty of
      INT => []
    | BOOL => []
    | VAR (var) => [var]
    | FUN (p, r) => (freeVars p) @ (freeVars r)

  fun containsVar ty v =
    case ty of
      INT => false
    | BOOL => false
    | VAR v' => v = v'
    | FUN (param, return) => containsVar param v orelse containsVar return v

  local
    val counter = ref 0
    fun increment r = !r before r := !r + 1
  in
    fun freshVar () = VAR (increment counter)
    fun resetFreshness () = counter := 0
  end

  fun toString INT = "int"
    | toString BOOL = "bool"
    | toString (VAR v) = "a" ^ Int.toString v
    | toString (FUN (p as FUN _, r)) = "(" ^ toString p ^ ") -> " ^ toString r
    | toString (FUN (p, r)) = toString p ^ " -> " ^ toString r
end

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

  structure GenVar =
  struct
    structure M = BinaryMapFn (
      struct
        type ord_key = int
        val compare = Int.compare
      end
    )

    val prefix = ref ""
    val counter = ref 0
    val varEnv : string M.map ref = ref M.empty

    fun genvar var =
      case M.find (!varEnv, var) of
        SOME v => v
      | NONE =>
        let
          val suffix = Char.toString (Char.chr (97 + !counter))
          val varName = !prefix ^ suffix
        in
          counter := !counter + 1;
          varEnv := M.insert (!varEnv, var, varName);
          varName
        end

    fun reset () = let in
      prefix := "";
      counter := 0;
      varEnv := M.empty
    end
  end

  local
    val counter = ref 0

    fun increment r = !r before r := !r + 1

    fun string INT = "int"
      | string (VAR v) = GenVar.genvar v
      | string (FUN (p as FUN _, r)) =
        let
          val return = string r
          val param = string p
        in
          "(" ^ param ^ ") -> " ^ return
        end
      | string (FUN (p, r)) = string p ^ " -> " ^ string r
  in
    fun freshVar () = VAR (increment counter)
    fun resetFreshness () = counter := 0
    fun toString ty = let in
      GenVar.reset ();
      string ty
    end
  end
end

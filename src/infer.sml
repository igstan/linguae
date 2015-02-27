structure Infer =
struct
  fun typeOf term tenv =
    let
      open Unify
      val _ = Type.resetFreshness ()
      val typedTerm = annotate term tenv
      val subst = unify (constrain typedTerm)
      val termTy = TypedTerm.typeOf typedTerm
    in
      Subst.apply subst termTy
    end handle
      Unify.UnificationFailure (a, b) =>
        raise Fail ("cannot unify "^ Type.toString a  ^" with "^ Type.toString b)

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

  fun typeSignature term tenv =
    let
      open Type
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
      GenVar.reset ();
      string (typeOf term tenv)
    end
end

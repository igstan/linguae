structure Infer =
struct
  fun typeOf term tenv =
    let
      open Unify
      val _ = Type.resetFreshness ()
      val termTy = Type.freshVar ()
      val subst = unify (constrain tenv termTy term)
    in
      TypeEnv.generalize tenv (Subst.apply subst termTy)
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

      fun typeString INT = "int"
        | typeString BOOL = "bool"
        | typeString (VAR v) = GenVar.genvar v
        | typeString (FUN (p as FUN _, r)) =
          let
            val return = typeString r
            val param = typeString p
          in
            "(" ^ param ^ ") -> " ^ return
          end
        | typeString (FUN (p, r)) = typeString p ^ " -> " ^ typeString r

      fun typeScheme (TypeScheme.ForAll (vars, ty)) =
        let
          val tsVars = String.concat (List.map GenVar.genvar vars)
          val tsType = typeString ty
        in
          case vars of
            [] => tsType
          | _ => "forall " ^ tsVars ^ ". " ^ tsType
        end
    in
      GenVar.reset ();
      typeScheme (typeOf term tenv)
    end
end

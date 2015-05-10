(**
 * A type-scheme represents a polymorphic type or polytype.
 *
 * Examples include:
 *
 *  - identity function: `∀a. a -> a`
 *  - constant function: `∀ab. a -> b -> a`
 *  - map function:      `∀ab. a list -> (a -> b) -> b list`
 *
 * ## Instantiation
 *
 * This abstraction is relevant only in the context of let-bound polymorphism,
 * where a given let binding may be used with different types, e.g.:
 *
 * ```sml
 * let
 *   val id = fn a => a
 * in
 *   if id true then
 *     id 1
 *   else
 *     id 2
 * end
 * ```
 *
 * The `id` binding used in the conditional test part of the `if` expression
 * must be considered as accepting a different type than the binding used in
 * the `then` branch and the binding used the `else` branch. If this were not
 * to happen, then unification would fail.
 *
 * For this reason, in the above example we'll have to generated three separate
 * types for the let-bound `id` function:
 *
 *  - id[if]:   ∀α. α -> α
 *  - id[then]: ∀β. β -> β
 *  - id[else]: ∀γ. γ -> γ
 *
 * This allows each binding occurrence to __instantiate__ its type variable to
 * a different concrete type (unknown initially):
 *
 *  - id[if]:   α = bool
 *  - id[then]: β = int
 *  - id[else]: γ = int
 *
 * What this effectively entails in the implementation is to replace each of
 * the type varibles associated with the type-scheme with freshly generated
 * type variables. In the example above, the non-instantiated signature:
 *
 * ```
 * ∀a. a -> a
 * ```
 *
 * has forced the production of three fresh type variables — α, β and γ — one
 * for each of the binding occurrences.
 *
 * ## Generalization
 *
 * A perfectly legitimate question one might raise at this point, is why do we
 * need type-schemes at all. Can't we simply take a normal type and replace all
 * type variables we found inside with fresh type variables? We can't, and to
 * see why not, consider this example:
 *
 * ```sml
 * let
 *   val const = fn y =>
 *     let
 *       val f = fn x => y
 *     in
 *       f
 *     end
 * in
 *   (const 1 'a') + 2
 * end
 * ```
 *
 * Without type-schemes, the above example won't type-check, even though it
 * makes perfect sense to pass the compiler. The reason is that the inferred
 * type of `f` is too lax. It is inferred to be `∀αβ. α -> β` and later on,
 * `β` won't be unifiable with `2`. The reason for this is that `β` isn't
 * a free variable that should be replaced instantiation. It is actually a
 * type variable __constrained__ by the type of `y`, which in our case will
 * be in turn constrained to be of type `int`.
 *
 * For this reason we have to be explicit about which type variables are
 * actually free inside a type and this is actually what type-schemes do.
 *
 * As we've just seen, we need a way of recording information about a type's
 * free variables with respect to the __environment__ in which that type is
 * defined. A type-scheme is precisely that recording and the process of moving
 * from a type to a type-scheme is called __generalization__.
 *
 * For technical reason, however, the generalization procedure is defined in
 * the `TypeEnv`, instead of `TypeScheme`, because circular dependencies are
 * disallowed in Standard ML.
 *)
structure TypeScheme =
struct
  structure Var =
  struct
    type ty = int
  end

  datatype ty =
    SVAR of Var.ty
  | FORALL of Type.Var.ty list * Type.ty

  fun instantiate typeScheme =
    case typeScheme of
      SVAR _ => raise Fail "TypeScheme VAR"
    | FORALL (tyVars, ty) =>
      let
        fun extend (var, subst) = Subst.set subst var (Type.freshVar ())
        val subst = List.foldl extend Subst.empty tyVars
      in
        Subst.apply subst ty
      end

  local
    structure Set = BinarySetFn (Type.Var.Key)
  in
    fun freeVars (SVAR _) = raise Fail "TypeScheme VAR"
      | freeVars (FORALL (tyVars, ty)) =
        let
          val tyFreeVars = Set.fromList (Type.freeVars ty)
          val quantFreeVars = Set.fromList tyVars
          val boundVars = Set.difference (tyFreeVars, quantFreeVars)
        in
          Set.listItems boundVars
        end
  end

  local
    val counter = ref 0
    fun increment r = !r before r := !r + 1
  in
    fun freshVar () = SVAR (increment counter)
    fun resetFreshness () = counter := 0
  end

  fun toString (SVAR var) = "SVAR " ^ Int.toString var
    | toString (FORALL (vars, ty)) =
      let
        fun string v = Type.toString (Type.VAR v)
        val forall = String.concat (List.map string vars)
        val prefix = if null vars then "" else "forall "^ forall ^". "
      in
        prefix ^ Type.toString ty
      end
end

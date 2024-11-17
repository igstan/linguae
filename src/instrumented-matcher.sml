(*) Described in §4 and §5 of the paper.
structure InstrumentedMatcher =
  struct
    open Common

    datatype termd =
      (*) Constructors known to match, along with their argument descriptions.
    | Pos of con * termd list
      (*) Constructors known to NOT match.
    | Neg of con list

    (**
     * Adds that a `con` can't match. Preconditions:
     *
     *  1. con ∉ nonset
     *  2. card(nonset ∪ {con}) < span(con)
     *)
    fun addneg (Neg nonset) con = Neg (con :: nonset)
      | addneg _ _              = raise Fail "illegal case: addneg"

    (**
     * While we match, the context gets transformed in three major ways:
     *
     *  1. We match successfully on a ctor's argument, so we push this
     *     knowledge at the top of the context.
     *  2. We fully matched a ctor and all its arguments, so we transform the
     *     in-progress head of the list into a `Pos` description and place
     *     that at the front of the list.
     *  3. We failed to match, so we must recompute the head of the context.
     *)
    type context = (con * termd list) list

    (**
     * Augments a ctor's (partial) list of arguments. The head of the list
     * holds the right-most visited argument.
     *)
    fun augment [] dsc                    = []
      | augment ((con, args) :: rest) dsc = (con, dsc :: args) :: rest

    (**
     * When we know all of a ctor's arguments, convert them into a Pos term
     * description and push it at the head of the rest of the context.
     *)
    fun norm ((con, args) :: rest) = augment rest (Pos (con, rev args))
      | norm _ = raise Fail "illegal case: norm"

    (**
     * This gets called on a failed match. It has to unwind the context up to
     * the root of the pattern and, as it climbs up, join together the left,
     * visited argument descs with the right, unvisited argument descs, under
     * a `Pos` occurrence of the constructor (because we know this matched
     * earlier if it ended up in the context list).
     *)
    fun builddsc [] dsc [] = dsc
      | builddsc ((con, args) :: rest) dsc ((_, _, dargs) :: work) =
          builddsc rest (Pos (con, rev args @ (dsc :: dargs))) work
      | builddsc _ _ _ = raise Fail "illegal case: builddsc"

    datatype matchresult = Yes | No | Maybe

    fun staticmatch ({name = pcon, span, ...} : con) (dsc : termd) =
      case dsc of
      | Pos ({name = ocon, ...}, _) => if pcon = ocon then Yes else No
      | Neg cons =>
          if List.exists (fn { name = ocon, ... } => ocon = pcon) cons
          then No
          else
            if span = 1 + List.length cons
            then Yes
            else Maybe

    fun main origobj allmrules =
      let
        fun fail dsc []                        = NONE
          | fail dsc ((pat1, rhs1) :: ruleset) = match pat1 origobj dsc [] [] rhs1 ruleset

        and succeed ctx [] rhs rules = SOME rhs
          | succeed ctx (work1 :: workr) rhs rules =
              case work1 of
              | ([], [], []) => succeed (norm ctx) workr rhs rules
              | (pat1 :: patr, obj1 :: objr, dsc1 :: dscr) =>
                  match pat1 obj1 dsc1 ctx ((patr, objr, dscr) :: workr) rhs rules
              | _ => raise Fail "illegal case: succeed"

        and match (PVar _) obj dsc ctx work rhs rules =
              succeed (augment ctx dsc) work rhs rules
          | match (PCon (pcon, pargs)) (PCon (ocon, oargs)) dsc ctx work rhs rules =
              let
                fun args f = List.tabulate (#arity pcon, f)

                fun getdargs (Neg _)            = args (fn _ => Neg [])
                  | getdargs (Pos (con, dargs)) = dargs

                fun succeed' () =
                  succeed
                    ((pcon, []) :: ctx)
                    ((pargs, oargs, getdargs dsc) :: work)
                    rhs
                    rules

                fun fail' newdsc = fail (builddsc ctx newdsc work) rules
              in
                case staticmatch pcon dsc of
                | Yes   => succeed' ()
                | No    => fail' dsc
                | Maybe => if ocon = pcon
                           then succeed' ()
                           else fail' (addneg dsc pcon)
              end
          | match _ _ _ _ _ _ _ = raise Fail "illegal case: match"
      in
        fail (Neg []) allmrules
      end

    fun test () =
      let
        val null     = PCon ({ name = "null",  arity = 0, span = 2 }, [])
        fun cons a b = PCon ({ name = "cons", arity = 2, span = 2 }, [a, b])
      in
        main (cons null null) [
          (null                            , 1),
          (cons null          null         , 2),
          (cons null          (PVar "rest"), 3),
          (cons (PVar "head") (PVar "rest"), 4)
        ]
      end
  end


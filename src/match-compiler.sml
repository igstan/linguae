(*) Described in §7 of the paper.
structure MatchCompiler =
  struct
    open Fn.Syntax infix |>
    open Common

    structure Access =
      struct
        datatype t = Obj | Sel of int * t

        fun toString (Obj)           = "Obj"
          | toString (Sel (i, rest)) = toString rest ^ "." ^ Int.toString i
      end

    structure Decision =
      struct
        datatype 'rhs t =
        | Failure
        | Success of 'rhs
        | Branch of Access.t * con * 'rhs t * 'rhs t

        fun toString t { rhs = action } =
          let
            fun indent n = "\n" ^ (String.repeat (2 * n) " ")
            fun loop t { level } =
              let
              in
                case t of
                | Failure     => "FAILURE"
                | Success rhs => "SUCCESS: " ^ action rhs
                | Branch (access, { name, arity, span }, a, b) =>
                    indent level ^ "WHEN: " ^ Access.toString access ^ " = " ^ name ^
                    indent level ^ "THEN: " ^ loop a { level = level + 1 } ^
                    indent level ^ "ELSE: " ^ loop b { level = level + 1 }
              end
          in
            loop t { level = 0 }
          end
      end

    datatype decision = datatype Decision.t

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

    fun compile allmrules =
      let
        fun fail dsc []                        = Failure
          | fail dsc ((pat1, rhs1) :: ruleset) = match pat1 Access.Obj dsc [] [] rhs1 ruleset

        and succeed ctx [] rhs rules = Success rhs
          | succeed ctx (work1 :: workr) rhs rules =
              case work1 of
              | ([], [], []) => succeed (norm ctx) workr rhs rules
              | (pat1 :: patr, obj1 :: objr, dsc1 :: dscr) =>
                  match pat1 obj1 dsc1 ctx ((patr, objr, dscr) :: workr) rhs rules
              | _ => raise Fail "illegal case: succeed"

        and match (PVar _) obj dsc ctx work rhs rules =
              succeed (augment ctx dsc) work rhs rules
          | match (PCon (pcon, pargs)) obj dsc ctx work rhs rules =
              let
                fun args f = List.tabulate (#arity pcon, f)

                fun getdargs (Neg _)            = args (fn _ => Neg [])
                  | getdargs (Pos (con, dargs)) = dargs

                fun getoargs () = args (fn i => Access.Sel (i + 1, obj))

                fun succeed' () =
                  succeed
                    ((pcon, []) :: ctx)
                    ((pargs, getoargs (), getdargs dsc) :: work)
                    rhs
                    rules

                fun fail' newdsc = fail (builddsc ctx newdsc work) rules
              in
                case staticmatch pcon dsc of
                | Yes   => succeed' ()
                | No    => fail' dsc
                | Maybe => Branch (obj, pcon, succeed' (), fail' (addneg dsc pcon))
              end
      in
        fail (Neg []) allmrules
      end

    fun test () =
      let
        val True  = PCon ({ name = "True",  arity = 0, span = 2 }, [])
        val False = PCon ({ name = "False", arity = 0, span = 2 }, [])

        val Red   = PCon ({ name = "Red",   arity = 0, span = 3 }, [])
        val Blue  = PCon ({ name = "Blue",  arity = 0, span = 3 }, [])
        val Green = PCon ({ name = "Green", arity = 0, span = 3 }, [])

        fun Pair a b = PCon ({ name = "Pair", arity = 2, span = 1 }, [a, b])

        val decisionTree1 = compile [
          (Pair True  Green, 1),
          (Pair False Green, 2),
          (Pair False Red,   3),
          (Pair False Blue,  4)
        ]
        (* The above compiles to:

        This is actually quite a good result, seeing how it forwent the test
        for case 4 and just returned it.

        WHEN: Obj.1 = True
        THEN:
          WHEN: Obj.2 = Green
          THEN: SUCCESS: 1
          ELSE: FAILURE
        ELSE:
          WHEN: Obj.2 = Green
          THEN: SUCCESS: 2
          ELSE:
            WHEN: Obj.2 = Red
            THEN: SUCCESS: 3
            ELSE: SUCCESS: 4 *)

        val decisionTree2 = compile [
          (Pair True  Green, 1),
          (Pair False Green, 2),
          (Pair False Red,   3),
          (Pair True  Blue,  4)
        ]
        (* The above compiles to:

        Once again, quite impressive seeing how it moved the test for `Blue`
        inside the sub-tree testing for `True`, effectively avoiding that
        test.

        Note: The presence of FAILURE indicates a non-exhaustive match.

        WHEN: Obj.1 = True
        THEN:
          WHEN: Obj.2 = Green
          THEN: SUCCESS: 1
          ELSE:
            WHEN: Obj.2 = Blue
            THEN: SUCCESS: 4
            ELSE: FAILURE
        ELSE:
          WHEN: Obj.2 = Green
          THEN: SUCCESS: 2
          ELSE:
            WHEN: Obj.2 = Red
            THEN: SUCCESS: 3
            ELSE: FAILURE *)
      in
        Decision.toString decisionTree1 {rhs=Int.toString} |> Console.println;
        Decision.toString decisionTree2 {rhs=Int.toString} |> Console.println
      end

    fun exponential () =
      let
        val v = PVar "_"
        val A = PCon ({ name = "A", arity = 0, span = 2 }, [])
        val B = PCon ({ name = "B", arity = 0, span = 2 }, [])

        fun T elems =
          PCon ({ name = "Pair", arity = List.length elems, span = 1 }, elems)

        val decisionTree = compile [
          (T [A,A,v,v,v,v,v,v,v,v],  0),
          (T [v,v,A,A,v,v,v,v,v,v],  1),
          (T [v,v,v,v,A,A,v,v,v,v],  2),
          (T [v,v,v,v,v,v,A,A,v,v],  3),
          (T [v,v,v,v,v,v,v,v,A,A],  4),
          (T [A,B,A,B,A,B,A,B,A,B], ~1)
        ]
      in
        Decision.toString decisionTree {rhs=Int.toString} |> Console.println
      end
  end

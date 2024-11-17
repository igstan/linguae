(* Follows: "ML pattern match compilation (March 5, 1996).pdf" *)

structure Common =
  struct
    type con = { name : string, arity : int, span : int }

    datatype pat =
    | PVar of string
    | PCon of con * pat list

    datatype 'a tree =
    | Null
    | Leaf of 'a
    | Node of 'a tree * 'a * 'a tree

    val Nullc = { name = "Null", arity = 0, span = 3 }
    val Leafc = { name = "Leaf", arity = 1, span = 3 }
    val Nodec = { name = "Node", arity = 3, span = 3 }

    type 'rhs match = (pat * 'rhs) list
  end

(*) Described in §3 of the paper.
structure NaiveMatcher =
  struct
    open Common

    fun main origobj allmrules =
      let
        fun fail []                         = NONE
          | fail ((pat1, rhs1) :: rulerest) = match pat1 origobj [] rhs1 rulerest

        and succeed work rhs rules =
          case work of
          | []                                    => SOME rhs
          | ([], [])                     :: workr => succeed workr rhs rules
          | (pat1 :: patr, obj1 :: objr) :: workr => match pat1 obj1 ((patr, objr) :: workr) rhs rules
          | _                                     => raise Fail "impossible: work"

        and match pattern object work rhs rules =
          case pattern of
          | PVar _ => succeed work rhs rules
          | PCon (pcon, pargs) =>
              case object of
              | PVar _ => raise Fail "impossible: PVar"
              | PCon (ocon, oargs) =>
                  if ocon = pcon
                  then succeed ((pargs, oargs) :: work) rhs rules
                  else fail rules
      in
        fail allmrules
      end

    fun test () =
      let
        val null     = PCon ({ name = "null",  arity = 0, span = 2 }, [])
        fun cons a b = PCon ({ name = "cons", arity = 2, span = 2 }, [a, b])
      in
        (*) So... this is a bit unusual because the strutinee is a pattern
        (*) too, whereas it should be a value.
        main (cons null (PVar "rest")) [
          (null, 1),
          (cons null          (PVar "rest"), 2),
          (cons (PVar "head") (PVar "rest"), 3)
        ]
      end
  end

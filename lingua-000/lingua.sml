type id = string

type table = (id * int) list

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

val prog =
  CompoundStm(
    AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(
      AssignStm("b", EseqExp(
        PrintStm [IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
        OpExp(NumExp 10, Times, IdExp"a")
      )),
      PrintStm[IdExp "b"]))

(* http://stackoverflow.com/a/9323417/58808  *)
(* http://stackoverflow.com/a/21205572/58808 *)
fun maxargs (s: stm): int =
  let fun loop count []             = count
        | loop count (top :: stack) =
            case top of
              PrintStm args                 => loop (Int.max (count, length(args))) stack
            | CompoundStm (a, b)            => loop count (a :: b :: stack)
            | AssignStm (_, EseqExp (a, _)) => loop count (a :: stack)
            | AssignStm _                   => loop count stack
  in
    loop 0 [s]
  end

fun lookup id table =
  #2 (valOf (List.find (fn (id', _) => id' = id) table))

fun interpStm statement t =
  case statement of
    CompoundStm (s1, s2) => interpStm s2 (interpStm s1 t)
  | AssignStm (id, e)    => let val (v, t') = interpExp e t in (id, v) :: t' end
  | PrintStm exps        => printArgs t exps

and interpExp exp t =
  case exp of
    IdExp id              => (lookup id t, t)
  | NumExp n              => (n, t)
  | OpExp (e1, binop, e2) => binOp e1 binop e2 t
  | EseqExp (s, e)        => interpExp e (interpStm s t)

and binOp e1 binop e2 t =
  let
    val (v1, t1) = interpExp e1 t
    val (v2, t2) = interpExp e2 t1
  in
    case binop of
      Plus  => (v1 + v2, t2)
    | Minus => (v1 - v2, t2)
    | Times => (v1 * v2, t2)
    | Div   => (v1 div v2, t2)
  end

and printArgs t exps =
  let
    fun folder (e, (acc, prefix)) =
      let val (v, t') = interpExp e acc
      in
        (t', " ") before print (prefix ^ (Int.toString v))
      end
    val result = foldl folder (t, "") exps
  in
    (#1 result) before print "\n"
  end

fun interp stm: unit = ignore (interpStm stm [])

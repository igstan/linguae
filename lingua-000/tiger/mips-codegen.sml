structure MipsCodegen : CODEGEN =
struct
  structure A = Assem
  structure T = Tree
  structure Frame = Frame

  fun codegen frame tree =
    let
      val instructions : A.instr list ref = ref []

      fun result gen = let val t = Temp.newTemp () in gen t ; t end

      fun munchStm stm =
        case stm of
          T.SEQ (a, b) => raise Fail "not implemented"
        | T.LABEL label => raise Fail "not implemented"
        | T.JUMP (exp, labels) => raise Fail "not implemented"
        | T.CJUMP (T.EQ, a, b, tLabel, fLabel) =>
            A.OPER {
              assem = "beq `s0, `s1, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GE, a, T.CONST 0, tLabel, fLabel) =>
            A.OPER {
              assem = "bgez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GT, a, T.CONST 0, tLabel, fLabel) =>
            A.OPER {
              assem = "bgtz `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LE, a, T.CONST 0, tLabel, fLabel) =>
            A.OPER {
              assem = "blez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (relop, a, b, tLabel, fLabel) => raise Fail "not implemented"
        | T.MOVE (dst, src) => raise Fail "not implemented"
        | T.EXP exp => raise Fail "not implemented"

      and munchExp exp =
        case exp of
          T.BINOP (binop, a, b) => raise Fail "not implemented"
        | T.MEM exp => raise Fail "not implemented"
        | T.TEMP temp => raise Fail "not implemented"
        | T.ESEQ (stm, exp) => raise Fail "not implemented"
        | T.NAME label => raise Fail "not implemented"
        | T.CONST c => raise Fail "not implemented"
        | T.CALL (func, args) => raise Fail "not implemented"
    in
      munchStm tree
    ; List.rev (!instructions)
    end
end

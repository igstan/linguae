structure MipsCodegen : CODEGEN =
struct
  infixr 0 $

  open Fn

  structure A = Assem
  structure T = Tree
  structure Frame = Frame

  fun immediate c =
    let
      val s = Int.toString (Int.abs c)
    in
      if c >= 0 then s else "-" ^ s
    end

  fun codegen frame tree =
    let
      val instructions : A.instr list ref = ref []

      fun withTemporary f = let val t = Temp.newTemp () in f t ; t end

      fun emit instruction = instructions := instruction :: (!instructions)

      fun munchStm stm =
        case stm of
          T.SEQ (a, b) => (munchStm a ; munchStm b)
        | T.LABEL label =>
            emit $ A.LABEL { assem = Symbol.name label ^ ":\n", lab = label }
        | T.JUMP (T.NAME label, _) =>
            emit $ A.OPER {
              assem = "j `j0",
              src = [],
              dst = [],
              jump = SOME [label]
            }
        | T.JUMP _ => raise Fail "bug: can only jump to labels"

        (* —————————————————————————————————————————————————————————————————— *)
        (* a == b                                                             *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.EQ, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "beq `s0, $zero, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.EQ, T.CONST 0, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.EQ, a, T.CONST 0, tLabel, fLabel))
        | T.CJUMP (T.EQ, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "beq `s0, `s1, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }

        (* —————————————————————————————————————————————————————————————————— *)
        (* a != b                                                             *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.NE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bne `s0, $zero, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.NE, T.CONST 0, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.NE, a, T.CONST 0, tLabel, fLabel))
        | T.CJUMP (T.NE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bne `s0, `s1, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }

        (* —————————————————————————————————————————————————————————————————— *)
        (* a >= b                                                             *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.GE, c as T.CONST _, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.LE, a, c, tLabel, fLabel))
        | T.CJUMP (T.GE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GE, a, T.CONST c, tLabel, fLabel) =>
          let
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "slti `d0, `s0, " ^ immediate c,
              src = [munchExp a],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.NE, T.TEMP condition, T.CONST 0, tLabel, fLabel))
          end
        | T.CJUMP (T.GE, a, b, tLabel, fLabel) =>
          let
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "slt `d0, `s0, `s1",
              src = [munchExp a, munchExp b],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.EQ, T.TEMP condition, T.CONST 0, tLabel, fLabel))
          end

        (* —————————————————————————————————————————————————————————————————— *)
        (* a > b                                                              *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.GT, c as T.CONST _, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.LT, a, c, tLabel, fLabel))
        | T.CJUMP (T.GT, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgtz `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GT, a, c as T.CONST _, tLabel, fLabel) =>
          let
            val tempResult = Temp.newTemp ()
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "addi `d0, $zero, $s0",
              src = [munchExp c],
              dst = [tempResult],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.LT, T.TEMP tempResult, a, tLabel, fLabel))
          end
        | T.CJUMP (T.GT, a, b, tLabel, fLabel) =>
          munchStm (T.CJUMP (T.LT, b, a, tLabel, fLabel))

        (* —————————————————————————————————————————————————————————————————— *)
        (* a <= b                                                             *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.LE, c as T.CONST _, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.GE, a, c, tLabel, fLabel))
        | T.CJUMP (T.LE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "blez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LE, a, T.CONST c, tLabel, fLabel) =>
          let
            val decrement = Temp.newTemp ()
            val condition = Temp.newTemp ()
          in
            (* `a <= b` is equivalent to `(a - 1) < b` *)
            emit $ A.OPER {
              assem = "addi `d0, `s0, 0xffffffff",
              src = [munchExp a],
              dst = [decrement],
              jump = SOME []
            }
          ; emit $ A.OPER {
              assem = "slti `d0, `s0, " ^ immediate c,
              src = [decrement],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.NE, T.TEMP condition, T.CONST 0, tLabel, fLabel))
          end
        | T.CJUMP (T.LE, a, b, tLabel, fLabel) =>
          let
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "slt `d0, `s1, `s0",
              src = [munchExp a, munchExp b],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.EQ, T.TEMP condition, T.CONST 0, fLabel, tLabel))
          end

        (* —————————————————————————————————————————————————————————————————— *)
        (* a < b                                                              *)
        (* —————————————————————————————————————————————————————————————————— *)
        | T.CJUMP (T.LT, c as T.CONST _, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.GT, a, c, tLabel, fLabel))
        | T.CJUMP (T.LT, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bltz `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LT, a, T.CONST c, tLabel, fLabel) =>
          let
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "slti `d0, `s0, " ^ immediate c,
              src = [munchExp a],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.NE, T.TEMP condition, T.CONST 0, tLabel, fLabel))
          end
        | T.CJUMP (T.LT, a, b, tLabel, fLabel) =>
          let
            val condition = Temp.newTemp ()
          in
            emit $ A.OPER {
              assem = "slt `d0, `s0, `s1",
              src = [munchExp a, munchExp b],
              dst = [condition],
              jump = SOME []
            }
          ; munchStm (T.CJUMP (T.NE, T.TEMP condition, T.CONST 0, tLabel, fLabel))
          end

        | T.CJUMP (T.ULT, a, b, tLabel, fLabel) => raise Fail "not implemented"
        | T.CJUMP (T.ULE, a, b, tLabel, fLabel) => raise Fail "not implemented"
        | T.CJUMP (T.UGT, a, b, tLabel, fLabel) => raise Fail "not implemented"
        | T.CJUMP (T.UGE, a, b, tLabel, fLabel) => raise Fail "not implemented"
        | T.MOVE (dst, src) => raise Fail "not implemented"
        | T.EXP exp => ignore (munchExp exp)

      and munchExp exp =
        case exp of
          T.BINOP (T.PLUS, T.CONST 0, a) => munchExp a
        | T.BINOP (T.PLUS, a, T.CONST 0) => munchExp a
        | T.BINOP (T.PLUS, c as T.CONST _, a) => munchExp (T.BINOP (T.PLUS, a, c))
        | T.BINOP (T.PLUS, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "addiu `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.PLUS, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "addu `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.MINUS, a, T.CONST 0) => munchExp a
        | T.BINOP (T.MINUS, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "subiu `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.MINUS, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "subu `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.MUL, T.CONST 1, a) => munchExp a
        | T.BINOP (T.MUL, a, T.CONST 1) => munchExp a
        | T.BINOP (T.MUL, c as T.CONST _, a) => munchExp (T.BINOP (T.MUL, a, c))
        | T.BINOP (T.MUL, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "mul `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.MUL, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "mul `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.DIV, a, T.CONST 1) => munchExp a
        | T.BINOP (T.DIV, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "div `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.DIV, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "div `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.AND, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "andi `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.AND, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "and `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.OR, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "ori `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.OR, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "or `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.LSHIFT, a, b) => raise Fail "not implemented"
        | T.BINOP (T.RSHIFT, a, b) => raise Fail "not implemented"
        | T.BINOP (T.ARSHIFT, a, b) => raise Fail "not implemented"
        | T.BINOP (T.XOR, a, b) => raise Fail "not implemented"
        | T.MEM exp => raise Fail "not implemented"
        | T.TEMP temp => temp
        | T.ESEQ (stm, exp) => (munchStm stm ; munchExp exp)
        | T.NAME label =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "la `d0, " ^ Symbol.name label,
                src = [],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.CONST c =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "addiu `d0, $zero, " ^ immediate c,
                src = [],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.CALL (func, args) => raise Fail "not implemented"
    in
      munchStm tree
    ; List.rev (!instructions)
    end
end

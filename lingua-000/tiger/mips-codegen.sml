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
        | T.JUMP (T.NAME label, labels) =>
            emit $ A.OPER {
              assem = "j `j0",
              src = [],
              dst = [],
              jump = SOME labels
            }
        | T.JUMP (T.TEMP temp, labels) =>
            emit $ A.OPER {
              assem = "jr `s0",
              src = [temp],
              dst = [],
              jump = SOME labels
            }
        | T.JUMP (target, labels) =>
            emit $ A.OPER {
              assem = "jr `s0",
              src = [munchExp target],
              dst = [],
              jump = SOME labels
            }
        | T.CJUMP (T.EQ, T.CONST 0, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.EQ, a, T.CONST 0, tLabel, fLabel))
        | T.CJUMP (T.EQ, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "beq `s0, $zero, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.EQ, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "beq `s0, `s1, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.NE, T.CONST 0, a, tLabel, fLabel) =>
            munchStm (T.CJUMP (T.NE, a, T.CONST 0, tLabel, fLabel))
        | T.CJUMP (T.NE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bne `s0, $zero, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.NE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bne `s0, `s1, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GE, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bge `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bge `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GT, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgtz `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GT, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgt `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.GT, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgt `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LE, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "blez `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LE, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "ble `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "ble `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LT, a, T.CONST 0, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bltz `s0, `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LT, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "blt `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.LT, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "blt `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.ULT, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bltu `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.ULT, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bltu `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.ULE, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bleu `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.ULE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bleu `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.UGT, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgtu `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.UGT, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgtu `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.UGE, a, T.CONST c, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgeu `d0, " ^ immediate c ^ ", `j0",
              src = [munchExp a],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.CJUMP (T.UGE, a, b, tLabel, fLabel) =>
            emit $ A.OPER {
              assem = "bgeu `d0, `s0, `j0",
              src = [munchExp a, munchExp b],
              dst = [],
              jump = SOME [tLabel, fLabel]
            }
        | T.MOVE (T.TEMP temp, T.CONST c) =>
            emit $ A.OPER {
              assem = "li `d0, " ^ immediate c,
              src = [],
              dst = [temp],
              jump = SOME []
            }
        | T.MOVE (T.TEMP temp, src) =>
            emit $ A.OPER {
              assem = "move `d0, `s0",
              src = [munchExp src],
              dst = [temp],
              jump = SOME []
            }
        | T.MOVE (src, T.CONST c) =>
          let
            val result = munchExp src
          in
            emit $ A.OPER {
              assem = "sw `d0, " ^ immediate c,
              src = [result],
              dst = [result],
              jump = SOME []
            }
          end
        | T.MOVE (T.BINOP (T.PLUS, c as T.CONST _, a), src) =>
            munchStm (T.MOVE (T.BINOP (T.PLUS, a, c), src))
        | T.MOVE (T.BINOP (T.PLUS, a, T.CONST c), src) =>
          let
            val dst = munchExp a
            val src = munchExp src
          in
            emit $ A.OPER {
              assem = "sw `d0, " ^ immediate c ^ "(`s0)",
              src = [dst, src],
              dst = [src],
              jump = SOME []
            }
          end
        | T.MOVE (T.BINOP (T.MINUS, a, T.CONST c), src) =>
          let
            val dst = munchExp a
            val src = munchExp src
          in
            emit $ A.OPER {
              assem = "sw `d0, " ^ immediate (~ c) ^ "(`s0)",
              src = [dst, src],
              dst = [dst],
              jump = SOME []
            }
          end
        | T.MOVE (T.MEM dst, src) =>
            munchStm (T.MOVE (dst, src))
        | T.MOVE (dst, src) =>
          let
            val dst = munchExp dst
            val src = munchExp src
          in
            emit $ A.OPER {
              assem = "sw `d0, (`s0)",
              src = [dst, src],
              dst = [dst],
              jump = SOME []
            }
          end
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
        | T.BINOP (T.LSHIFT, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "sll `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.LSHIFT, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "sllv `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.RSHIFT, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "srl `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.RSHIFT, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "srlv `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.ARSHIFT, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "sra `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.ARSHIFT, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "srav `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.XOR, a, T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "xori `d0, `s0, " ^ immediate c,
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.BINOP (T.XOR, a, b) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "xor `d0, `s0, `s1",
                src = [munchExp a, munchExp b],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.MEM (T.CONST c) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "lw `d0, " ^ immediate c,
                src = [],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.MEM (T.BINOP (T.PLUS, a, c as T.CONST _)) =>
            munchExp (T.MEM (T.BINOP (T.PLUS, c, a)))
        | T.MEM (T.BINOP (T.PLUS, T.CONST c, a)) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "lw `d0, " ^ immediate c ^ "(`s0)",
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.MEM (T.BINOP (T.MINUS, a, T.CONST c)) =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "lw `d0, " ^ immediate (~ c) ^ "(`s0)",
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
        | T.MEM a =>
            withTemporary (fn temp =>
              emit $ A.OPER {
                assem = "lw `d0, (`s0)",
                src = [munchExp a],
                dst = [temp],
                jump = SOME []
              }
            )
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
        | T.CALL (T.NAME label, args) =>
          let
            val calldefs = [Frame.RV, Frame.RA] @ Frame.Registers.callerSave
          in
            emit $ A.OPER {
              assem = "jal " ^ Symbol.name label,
              src = munchArgs args,
              dst = calldefs,
              jump = SOME []
            }
          ; Frame.RV
          end
        | T.CALL (func, args) =>
          let
            val calldefs = [Frame.RV, Frame.RA] @ Frame.Registers.callerSave
          in
            emit $ A.OPER {
              assem = "jalr `s0",
              src = (munchExp func) :: munchArgs args,
              dst = calldefs,
              jump = SOME []
            }
          ; Frame.RV
          end

      and munchArgs args =
        let
          fun loop i args argRegisters usedTemps =
            case (args, argRegisters) of
              ([], _) => usedTemps
            | (arg :: args, argRegister :: argRegisters) =>
              let in
                emit $ A.OPER {
                  assem = "move `d0, `s0",
                  src = [munchExp arg],
                  dst = [argRegister],
                  jump = SOME []
                }
              ; loop (i + 1) args argRegisters (argRegister :: usedTemps)
              end
            | (arg :: args, []) => (* Pass arguments via the stack. *)
              let in
                emit $ A.OPER {
                  (*
                   * Because the stack grows from higher to lower address and
                   * the arguments are stored closer to the stack pointer, the
                   * offsets are positive, not negative.
                   *)
                  assem = "sw `s0, " ^ immediate (i * Frame.wordSize) ^ "(`d0)",
                  src = [munchExp arg],
                  dst = [Frame.SP],
                  jump = SOME []
                }
              ; loop (i + 1) args [] usedTemps
              end
        in
          List.rev (loop 0 args Frame.Registers.arguments [])
        end
    in
      munchStm tree
    ; List.rev (!instructions)
    end
end

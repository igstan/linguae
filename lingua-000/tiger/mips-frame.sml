structure MipsFrame :> FRAME =
struct
  infix 1 |>

  open Fn
  structure A = Assem
  structure T = Tree
  structure R = MipsRegister

  (**
   * Where this local will be stored — stack frame or register.
   *)
  datatype access =
    InFrame of int          (* At offset X from the frame pointer. *)
  | InRegister of Temp.temp (* A register name. *)

  datatype frame =
    StackFrame of {
      (* Address of the function that this frame belongs to. *)
      label : Temp.label,
      (* Formal parameter accesses. *)
      formals : access list,
      (* Total number of formals and locals this frame stores. *)
      inFrameCount : int ref
      (* TODO: implement view-shift *)
    }

  datatype frag =
    PROC of { body : Tree.stm, frame : frame }
  | STRING of Temp.label * string

  type register = string

  val SP = R.SP
  val FP = R.FP
  val RV = R.V0
  val RA = R.RA
  val wordSize = 4

  structure TempMap = BinaryMapFn(struct
    type ord_key = Temp.temp
    val compare = Int.compare
  end)

  val tempMap =
    let
      fun enter ((k, v), table) = TempMap.insert (table, k, v)
    in
      List.foldl enter TempMap.empty [
        (R.ZERO, "$zero"),
        (R.V0, "$v0"),
        (R.V1, "$v1"),
        (R.A0, "$a0"),
        (R.A1, "$a1"),
        (R.A2, "$a2"),
        (R.A3, "$a3"),
        (R.T0, "$t0"),
        (R.T1, "$t1"),
        (R.T2, "$t2"),
        (R.T3, "$t3"),
        (R.T4, "$t4"),
        (R.T5, "$t5"),
        (R.T6, "$t6"),
        (R.T7, "$t7"),
        (R.T8, "$t8"),
        (R.T9, "$t9"),
        (R.S0, "$s0"),
        (R.S1, "$s1"),
        (R.S2, "$s2"),
        (R.S3, "$s3"),
        (R.S4, "$s4"),
        (R.S5, "$s5"),
        (R.S6, "$s6"),
        (R.S7, "$s7"),
        (R.SP, "$sp"),
        (R.FP, "$fp"),
        (R.RA, "$ra")
      ]
    end

  fun tempName temp =
    case TempMap.find (tempMap, temp) of
      NONE => Temp.makeString temp
    | SOME name => name

  structure Registers =
  struct
    val special = [R.V0, R.FP, R.SP, R.RA, R.ZERO]
    val arguments = [R.A0, R.A1, R.A2, R.A3]
    val calleeSave = [R.S0, R.S1, R.S2, R.S3, R.S4, R.S5, R.S6, R.S7]
    val callerSave = [R.T0, R.T1, R.T2, R.T3, R.T4, R.T5, R.T6, R.T7, R.T8, R.T9]
  end

  fun string (label, string) = string

  fun name (StackFrame { label, ... }) = Symbol.name label

  fun exp access framePointer =
    case access of
      InFrame offset => T.MEM (T.BINOP (T.PLUS, framePointer, T.CONST offset))
    | InRegister temp => T.TEMP temp

  fun externalCall (name, args) =
    (* TODO: verify we don't need to adjust the label name *)
    T.CALL (T.NAME (Temp.namedLabel name), args)

  (*
   * The MIPS calling conventions reserves 4 registers for procedure arguments,
   * from $a0 to $a3. Extra arguments will be passed in the stack frame.
   *)
  val maxFormalsInRegisters = 4

  val outermost = StackFrame {
    label = Temp.newLabel (),
    formals = [],
    inFrameCount = ref 0
  }

  (*
   * Allocates a new stack frame for a function named `name`. The `formals`
   * list tells which of the arguments, identified by position, escapes.
   *)
  fun newFrame { name, formals } =
    let
      (*
       * The argument count starts from 3, because the minimum stack frame
       * layout on MIPS must include space for the four arguments passed
       * via registers. Also, we're indexing from 0.
       *)
      val frameArgsCount = ref maxFormalsInRegisters

      (* Put first four arguments in registers, the rest in the frame. *)
      fun allocFormal (escapes, index) =
        if index < maxFormalsInRegisters
        (*
         * MIPS calling conventions reserve four registers for arguments.
         * Please note that we don't allocate any of the four arguments on
         * the stack, even if they escape, because the same calling conventions
         * mandate a frame layout which already accomodates spilling of the
         * four arguments.
         *
         * The minimum stack frame size on MIPS is 24 bytes. Four slots are
         * reserved for the four arguments passed in registers, plus 8 bytes
         * of padding because the stack pointer must be 8-byte aligned.
         *)
        then InRegister (Temp.newTemp ())
        (* The rest of the arguments will have to be passed on the stack. *)
        else InFrame (Ref.getAndIncrement frameArgsCount)

      fun allocFormals formals =
        ListPairs.zipWithIndex formals |> List.map allocFormal
    in
      StackFrame {
        label = name,
        formals = allocFormals formals,
        inFrameCount = frameArgsCount
      }
    end

  fun formals (StackFrame { formals, ... }) = formals

  fun allocLocal (StackFrame { inFrameCount, ... }) escapes =
    if escapes
    then InFrame (Ref.getAndIncrement inFrameCount)
    else InRegister (Temp.newTemp ())

  type proc = {
    prolog : string,
    body : Assem.instr list,
    epilog : string
  }

  fun procEntryExit1 (frame, body) = body

  fun procEntryExit2 (frame, body) =
    body @ [
      A.OPER {
        assem = "",
        src = [R.ZERO, R.RA, R.SP] @ Registers.calleeSave,
        dst = [],
        jump = SOME []
      }
    ]

  fun procEntryExit3 (StackFrame { label, ... }, body) = {
    prolog = "PROCEDURE " ^ Symbol.name label ^ "\n",
    body = body,
    epilog = "END " ^ Symbol.name label ^ "\n"
  }
end

structure Translate :> TRANSLATE =
struct
  open Fn
  infix 1 |>

  structure T = Tree
  structure Frame = Frame

  type frag = Frame.frag
  type offset = int

  datatype level =
    Top
  | Level of { index : int, parent : level, frame : Frame.frame }

  type access = level * Frame.access

  datatype exp =
    Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of Temp.label * Temp.label -> Tree.stm

  val fragments : frag list ref = ref []

  (*
   * Top level; inhabited by primitive functions and variables.
   *)
  val topLevel = Top

  (*
   * Creates a new stack frame; handles static links behind the scenes.
   *)
  fun newLevel { parent, name, formals } =
    let
      val index = case parent of
        Top => 0
      | Level { index, ... } => index + 1
    in
      Level {
        index = index,
        parent = parent,
        frame = Frame.newFrame { name = name, formals = true :: formals }
      }
    end

  (*
   * Extracts data about formal params from a Level-wrapped frame. Its main
   * job is to remove the static link parameter from the list of formals.
   *)
  fun formals level =
    case level of
      Top => []
    | Level { frame, ... } =>
        List.map (fn a => (level, a)) (Frame.formals frame) |> List.tl

  fun allocLocal level escapes =
    case level of
      Top => (Top, Frame.allocLocal Frame.outermost escapes)
    | Level { frame, ... } => (level, Frame.allocLocal frame escapes)

  fun unEx exp =
    case exp of
      Ex e => e
    | Nx s => T.ESEQ (s, T.CONST 0)
    | Cx genstm =>
      let
        val r = Temp.newTemp ()
        val t = Temp.newLabel ()
        val f = Temp.newLabel ()
      in
        T.ESEQ (
          T.seq [
            T.MOVE (T.TEMP r, T.CONST 1),
            genstm (t, f),
            T.LABEL f,
            T.MOVE (T.TEMP r, T.CONST 0),
            T.LABEL t
          ],
          T.TEMP r
        )
      end

  fun unNx exp =
    case exp of
      Nx s => s
    | Ex e => T.EXP e
    | Cx f =>
      let
        val label = Temp.newLabel ()
      in
        T.SEQ (f (label, label), T.LABEL label)
      end

  fun unCx exp =
    case exp of
      Cx f => f
    | Ex (T.CONST 0) =>
      (*
       * Parentheses need here and below because the parser is greey and will
       * parse the following rules of the case expressions as being part of the
       * fn expression.
       *)
      (fn (t, f) => T.JUMP (T.NAME f, [f]))
    | Ex (T.CONST 1) => (fn (t, f) => T.JUMP (T.NAME t, [t]))
    | Ex e => (fn (t, f) => T.CJUMP (T.EQ, e, T.CONST 1, t, f))
    | Nx s =>
      (*
       * The book says there's no need to treat this case, because well-typed
       * Tiger programs can't produce Cx(Nx) combinations, but it seems better
       * to keep the IR free from assumptions about the surface language.
       * Additionally, we already treat Ex(Nx) as producing CONST 0, so we may
       * just as well be consistent with that here.
       *)
      fn (t, f) => T.SEQ (s, T.JUMP (T.NAME f, [f]))

  val nilExp = Ex (T.CONST 0)

  fun simpleVar (access, level) = raise Fail "not implemented"
  fun fieldVar (record, offset) = raise Fail "not implemented"
  fun subscriptVar (array, offset) = raise Fail "not implemented"

  fun callExp (label, funLevel, callerLevel, args) =
    let
      fun chaseStaticLink (n, parent) =
        case (n, parent) of
          (0, _) => T.TEMP Frame.FP
        | (_, Top) => raise Fail "impossible: can't have Top with a non-zero level index"
        | (n, Level { parent, frame, ... }) =>
          (*
           * The location where the static link is stored is implementation
           * dependent, so the Frame module must be asked to calculate its
           * offset from the frame pointer.
           *)
          Frame.exp (hd (Frame.formals frame)) (chaseStaticLink (n - 1, parent))

      val staticLink =
        case funLevel of
          Top => Frame.externalCall (Symbol.name label, List.map unEx args)
        | Level { index = funIndex, ... } =>
          case callerLevel of
            Top => raise Fail "impossible: translation of a nested external function call"
          | Level { index = callerIndex, ... } => chaseStaticLink (callerIndex - funIndex, callerLevel)
    in
      Ex (T.CALL (T.NAME label, staticLink :: (List.map unEx args)))
    end

  fun opExp (oper, left, right) =
    let
      val l = unEx left
      val r = unEx right
    in
      case oper of
        Ast.PlusOp => Ex (T.BINOP (T.PLUS, l, r))
      | Ast.MinusOp => Ex (T.BINOP (T.MINUS, l, r))
      | Ast.TimesOp => Ex (T.BINOP (T.MUL, l, r))
      | Ast.DivideOp => Ex (T.BINOP (T.DIV, l, r))
      | Ast.EqOp => Cx (fn (t, f) => T.CJUMP (T.EQ, l, r, t, f))
      | Ast.NeqOp => Cx (fn (t, f) => T.CJUMP (T.NE, l, r, t, f))
      | Ast.LtOp => Cx (fn (t, f) => T.CJUMP (T.LT, l, r, t, f))
      | Ast.LeOp => Cx (fn (t, f) => T.CJUMP (T.LE, l, r, t, f))
      | Ast.GtOp => Cx (fn (t, f) => T.CJUMP (T.GT, l, r, t, f))
      | Ast.GeOp => Cx (fn (t, f) => T.CJUMP (T.GE, l, r, t, f))
    end

  (*
   * We need to allocate enough memory to hold n words, where n is the number
   * of fields. Then, we need to initialize those words by assigning them
   * the value of the translated expressions in `fieldExps`.
   *)
  fun recordExp fieldExps =
    let
      val size = Frame.wordSize * (List.length fieldExps)
      val r = T.TEMP (Temp.newTemp ())
      val start = T.MOVE (r, Frame.externalCall ("allocRecord", [T.CONST size]))
      fun initialize (fieldExp, index) =
        let
          val offset = T.MEM (T.BINOP (T.PLUS, r, T.CONST (index * Frame.wordSize)))
        in
          T.MOVE (offset, unEx fieldExp)
        end
      val initializeFields = ListPair.zipWithIndex fieldExps |> List.map initialize
    in
      Ex (T.ESEQ (T.seq (start :: initializeFields), r))
    end

  fun seqExp exps = raise Fail "not implemented"

  fun assignExp (destination, value) =
    Nx (T.MOVE (T.MEM (unEx destination), T.MEM (unEx value)))

  (* TODO: optimize when branches are Nx or Cx nodes. *)
  fun ifExp (test, thn, els) =
    let
      val thenLabel = Temp.newLabel ()
      val elseLabel = Temp.newLabel ()
      val doneLabel = Temp.newLabel ()
      val result = T.TEMP (Temp.newTemp ())
      val elseInstructions =
        case els of
          NONE => []
        | SOME els => [
            T.JUMP (T.NAME doneLabel, [doneLabel]),
            T.LABEL elseLabel,
            T.MOVE (result, unEx els),
            T.JUMP (T.NAME doneLabel, [doneLabel]),
            T.LABEL doneLabel
          ]
      val instructions = [
        unCx test (thenLabel, elseLabel),
        T.LABEL thenLabel,
        T.MOVE (result, unEx thn)
      ] @ elseInstructions
    in
      Ex (T.ESEQ (T.seq instructions, result))
    end

  (*
   * Translate while loops using the two-test approach described in Engineering
   * a Compiler, § 7.8.2. There, it's being argued that this form allows for
   * more optimizations as it creates a single basic block, instead of two.
   *
   * Additionaly, putting the loop test at the bottom helps with the translation
   * of "for" expressions, where care must be taken to not overflow the loop var
   * when incrementing it.
   *)
  fun whileExp (test, body, doneLabel) =
    let
      val bodyLabel = Temp.newLabel ()
      val instructions = [
        unCx test (bodyLabel, doneLabel),
        T.LABEL bodyLabel,
        unNx body,
        unCx test (bodyLabel, doneLabel),
        T.LABEL doneLabel
      ]
    in
      Nx (T.seq instructions)
    end

  fun forExp (lo, hi, body, breakLabel) = raise Fail "not implemented"
  fun letExp (decs, body) = raise Fail "not implemented"

  fun arrayExp (size, init) =
    Ex (Frame.externalCall ("initArray", [unEx size, unEx init]))

  fun breakExp label = Nx (T.JUMP (T.NAME label, [label]))

  fun intExp i = Ex (T.CONST i)

  fun stringExp string =
    let
      val label = Temp.newLabel ()
      val frag = Frame.STRING (label, string)
    in
      fragments := frag :: !fragments
    ; Ex (T.NAME label)
    end

  fun varDec init = raise Fail "not implemented"
  fun funDec (level, body) = raise Fail "not implemented"

  fun procEntryExit { level, body } = raise Fail "not implemented"
  fun getResult () = !fragments
end

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

  fun recordExp length = raise Fail "not implemented"
  fun seqExp exps = raise Fail "not implemented"

  fun assignExp (destination, value) =
    Nx (T.MOVE (T.MEM (unEx destination), T.MEM (unEx value)))

  fun ifExp (test, thn, els) = raise Fail "not implemented"
  fun whileExp (test, body, breakLabel) = raise Fail "not implemented"
  fun forExp (lo, hi, body, breakLabel) = raise Fail "not implemented"
  fun letExp (decs, body) = raise Fail "not implemented"
  fun arrayExp (size, init) = raise Fail "not implemented"

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

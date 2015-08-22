structure Translate :> TRANSLATE =
struct
  open Fn
  infix 1 |>

  structure T = Tree

  datatype level =
    Top
  | Level of { index : int, frame : Frame.frame }

  type access = level * Frame.access

  datatype exp =
    Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of Temp.label * Temp.label -> Tree.stm

  (*
   * Top level; inhabited by primitive functions and variables.
   *)
  val topLevel = Top

  (*
   * Creates a new stack frame; handles static links behind the scenes.
   *)
  fun newLevel { parent, name, formals } =
    let
      val level = case parent of
        Top => 0
      | Level { index, ... } => index + 1
    in
      Level {
        index = level,
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
end

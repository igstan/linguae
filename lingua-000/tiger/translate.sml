structure Translate :> TRANSLATE =
struct
  open Fn
  infix 1 |>

  datatype level =
    Outermost
  | Level of { index : int, frame : Frame.frame }

  type access = level * Frame.access

  (*
   * Top level; inhabited by primitive functions and variables.
   *)
  val outermost = Outermost

  (*
   * Creates a new stack frame; handles static links behind the scenes.
   *)
  fun newLevel { parent, name, formals } =
    let
      val level = case parent of
        Outermost => 0
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
      Outermost => []
    | Level { frame, ... } =>
        List.map (fn a => (level, a)) (Frame.formals frame) |> List.tl

  fun allocLocal level escapes =
    case level of
      Outermost => (Outermost, Frame.allocLocal Frame.outermost escapes)
    | Level { frame, ... } => (level, Frame.allocLocal frame escapes)
end

structure Assem =
struct
  infix 1 |>

  open Fn

  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr =
    OPER of {
      assem : string,
      dst : temp list,
      src : temp list,
      jump : label list option
    }
  | LABEL of {
      assem : string,
      lab : Temp.label
    }
  | MOVE of {
      assem : string,
      dst : temp,
      src : temp
    }

  fun format saytemp =
    let
      val saylab = Symbol.name
      (**
       * Replaces `s, `d and `j placeholders with values calculated by in the
       * child expressions.
       *)
      fun speak (assem, dst, src, jump) =
        let
          fun replace instruction =
            case instruction of
              #"`" :: #"s" :: i :: rest => explode (saytemp (List.nth (src, ord i - ord #"0"))) @ replace rest
            | #"`" :: #"d" :: i :: rest => explode (saytemp (List.nth (dst, ord i - ord #"0"))) @ replace rest
            | #"`" :: #"j" :: i :: rest => explode (saylab (List.nth (jump, ord i - ord #"0"))) @ replace rest
            | #"`" :: #"`" :: rest => #"`" :: replace rest
            | #"`" :: _ :: rest => raise Fail "bad assem format"
            | c :: rest => c :: replace rest
            | [] => []
        in
          assem |> explode |> replace |> implode
        end
    in
      fn OPER { assem, dst, src, jump } => speak (assem, dst, src, Option.getOpt (jump, []))
       | LABEL { assem, ... } => assem
       | MOVE { assem, dst, src } => speak (assem, [dst], [src], [])
    end
end

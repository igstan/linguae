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
          fun f instruction =
            case instruction of
              #"`" :: #"s" :: i :: rest => explode (saytemp (List.nth (src, ord i - ord #"0"))) @ f rest
            | #"`" :: #"d" :: i :: rest => explode (saytemp (List.nth (dst, ord i - ord #"0"))) @ f rest
            | #"`" :: #"j" :: i :: rest => explode (saylab (List.nth (jump, ord i - ord #"0"))) @ f rest
            | #"`" :: #"`" :: rest => #"`" :: f rest
            | #"`" :: _ :: rest => raise Fail "bad assem format"
            | c :: rest => c :: f rest
            | [] => []
        in
          assem |> explode |> f |> implode
        end
    in
      fn OPER { assem, dst, src, jump = NONE } => speak (assem, dst, src, [])
       | OPER { assem, dst, src, jump = SOME j } => speak (assem, dst, src, j)
       | LABEL { assem, ... } => assem
       | MOVE { assem, dst, src } => speak (assem, [dst], [src], [])
    end
end

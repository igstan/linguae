structure TreePrinter :> TREE_PRINTER =
struct
  structure T = Tree

  fun print (outstream, s0) =
    let
      fun say s = TextIO.output (outstream, s)
      fun sayln s = (say s ; say "\n")

      fun indent 0 = ()
        | indent i = (say " " ; indent(i-1))

      fun stm (T.SEQ (a, b), d) =
          let in
            indent d
          ; sayln "SEQ ("
          ; stm (a, d + 1)
          ; sayln ", "
          ; stm (b, d + 1)
          ; say ")"
          end
        | stm (T.LABEL lab, d) =
          let in
            indent d
          ; say "LABEL "
          ; say (Symbol.name lab)
          end
        | stm (T.JUMP (e,_), d) =
          let in
            indent d
          ; sayln "JUMP ("
          ; exp (e, d + 1)
          ; say ")"
          end
        | stm (T.CJUMP(r, a, b, t, f) ,d) =
          let in
            indent d
          ; say "CJUMP ("
          ; relop r
          ; sayln ", "
          ; exp (a, d + 1)
          ; sayln ", "
          ; exp (b, d + 1)
          ; sayln ", "
          ; indent (d + 1)
          ; say (Symbol.name t)
          ; say ","
          ; say (Symbol.name f)
          ; say ")"
          end
        | stm (T.MOVE(a, b), d) =
          let in
            indent d
          ; sayln "MOVE ("
          ; exp (a, d + 1)
          ; sayln ", "
          ; exp(b, d + 1)
          ; say ")"
          end
        | stm (T.EXP e, d) =
          let in
            indent d
          ; sayln "EXP ("
          ; exp (e, d + 1)
          ; say ")"
          end

      and exp (T.BINOP (p, a, b), d) =
          let in
            indent d
          ; say "BINOP ("
          ; binop p
          ; sayln ", "
          ; exp (a, d + 1)
          ; sayln ", "
          ; exp (b, d + 1)
          ; say ")"
          end
        | exp (T.MEM e, d) =
          let in
            indent d
          ; sayln "MEM ("
          ; exp (e, d + 1)
          ; say ")"
          end
        | exp (T.TEMP t, d) =
          let in
            indent d
          ; say "TEMP t"
          ; say(Int.toString t)
          end
        | exp (T.ESEQ (s, e), d) =
          let in
            indent d
          ; sayln "ESEQ ("
          ; stm (s, d + 1)
          ; sayln ", "
          ; exp (e, d + 1)
          ; say ")"
          end
        | exp (T.NAME lab, d) =
          let in
            indent d
          ; say "NAME "
          ; say (Symbol.name lab)
          end
        | exp (T.CONST i, d) =
          let in
            indent d
          ; say "CONST "
          ; say (Int.toString i)
          end
        | exp (T.CALL (e, el), d) =
          let in
            indent d
          ; sayln "CALL ("
          ; exp (e, d + 1)
          ; List.app (fn a => (sayln ", " ; exp (a, d + 2))) el
          ; say ")"
          end

      and binop a =
        case a of
          T.PLUS => say "PLUS"
        | T.MINUS => say "MINUS"
        | T.MUL => say "MUL"
        | T.DIV => say "DIV"
        | T.AND => say "AND"
        | T.OR => say "OR"
        | T.LSHIFT => say "LSHIFT"
        | T.RSHIFT => say "RSHIFT"
        | T.ARSHIFT => say "ARSHIFT"
        | T.XOR => say "XOR"

      and relop a =
        case a of
          T.EQ => say "EQ"
        | T.NE => say "NE"
        | T.LT => say "LT"
        | T.GT => say "GT"
        | T.LE => say "LE"
        | T.GE => say "GE"
        | T.ULT => say "ULT"
        | T.ULE => say "ULE"
        | T.UGT => say "UGT"
        | T.UGE => say "UGE"
  in
    stm (s0, 0)
  ; sayln ""
  ; TextIO.flushOut outstream
  end
end

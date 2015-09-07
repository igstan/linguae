structure Main =
struct
  fun emitproc out (Frame.STRING (lab, s)) = TextIO.output (out, Frame.string (lab, s))
    | emitproc out (Frame.PROC { body, frame }) =
      let
        val _ = print ("emit " ^ Frame.name frame ^ "\n")
        val _ = TreePrinter.print (out, body)
        val stms = Canon.linearize body
        val _ = app (fn s => TreePrinter.print (out, s)) stms
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
        val instrs = List.concat (map (MipsCodegen.codegen frame) stms')
        val format0 = Assem.format (Temp.makeString)
      in
        List.app (fn i => TextIO.output (out, format0 i)) instrs
      end

  fun withOpenFile fname f =
    let
      val out = TextIO.openOut fname
    in
      (f out before TextIO.closeOut out) handle
        e => (TextIO.closeOut out ; raise e)
    end

  fun compile filename =
    let
      val absyn = Parse.parse filename
      val frags = (EscapeAnalysis.analyse absyn ; Semant.translateProgram absyn)
    in
      withOpenFile (filename ^ ".s") (fn out => app (emitproc out) frags)
    end
end

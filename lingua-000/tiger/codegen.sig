signature CODEGEN =
sig
  structure Frame : FRAME

  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

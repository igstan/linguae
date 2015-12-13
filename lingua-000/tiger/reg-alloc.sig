signature REG_ALLOC =
sig
  structure Frame : FRAME
  structure TempMap : ORD_MAP where type Key.ord_key = Temp.temp

  type allocation = Frame.register TempMap.map

  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

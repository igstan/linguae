signature FRAME =
sig
  (**
   * Holds information about formal parameters and local variables allocated in
   * the frame:
   *
   * - the locations of all the formals
   * - instructions required to implement the "view shift"
   * - the number of locals allocated so far
   * - the label at which the function's machine code is to begin
   *)
  type frame

  (**
   * Describes where formals or locals are stored — frame or register.
   *)
  type access

  type register = string

  datatype frag =
    PROC of { body : Tree.stm, frame : frame }
  | STRING of Temp.label * string

  (**
   * The stack pointer.
   *)
  val SP : Temp.temp

  (**
   * The frame pointer.
   *)
  val FP : Temp.temp

  (**
   * The return value of a function.
   *)
  val RV : Temp.temp

  (**
   * The return address of a function.
   *)
  val RA : Temp.temp

  (**
   * Machine's word size.
   *)
  val wordSize : int

  structure TempMap : ORD_MAP where type Key.ord_key = Temp.temp

  val tempMap : register TempMap.map

  val tempName : Temp.temp -> string

  structure Registers : sig
    val special : Temp.temp list
    val arguments : Temp.temp list
    val callerSave : Temp.temp list
    val calleeSave : Temp.temp list
  end

  val outermost : frame

  val string : Temp.label * string -> string

  val name : frame -> string

  val exp : access -> Tree.exp -> Tree.exp

  val externalCall : string * Tree.exp list -> Tree.exp

  (**
   * For each formal parameter, this function must calculate two things:
   *
   * - how the parameter will be seen from inside the function (in a register
   *   or in a frame location)
   * - what instructions must be produced to implement the "view shift"
   *
   * For example, a frame-resident parameter will be seen as "memory at offset
   * X from the frame pointer", and the view shift will be implemented by
   * copying the stack pointer to the frame pointer on entry to the procedure.
   *)
  val newFrame : { name : Temp.label, formals : bool list } -> frame

  val formals : frame -> access list

  (**
   * The boolean parameter tells whether the local escapes.
   *)
  val allocLocal : frame -> bool -> access

  type proc = {
    prolog : string,
    body : Assem.instr list,
    epilog : string
  }

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> proc
end

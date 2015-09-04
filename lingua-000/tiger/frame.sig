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

  datatype frag =
    PROC of { body : Tree.stm, frame : frame }
  | STRING of Temp.label * string

  (**
   * The frame pointer.
   *)
  val FP : Temp.temp

  (**
   * The return value of a function.
   *)
  val RV : Temp.temp

  (**
   * Machine's word size.
   *)
  val wordSize : int

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

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
end

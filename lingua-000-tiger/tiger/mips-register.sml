structure MipsRegister =
struct
  val ZERO = Temp.newTemp ()

  (* return value registers *)
  val V0 = Temp.newTemp ()
  val V1 = Temp.newTemp ()

  (* argument registers; callee-saved *)
  val A0 = Temp.newTemp ()
  val A1 = Temp.newTemp ()
  val A2 = Temp.newTemp ()
  val A3 = Temp.newTemp ()

  (* caller-saved registers *)
  val T0 = Temp.newTemp ()
  val T1 = Temp.newTemp ()
  val T2 = Temp.newTemp ()
  val T3 = Temp.newTemp ()
  val T4 = Temp.newTemp ()
  val T5 = Temp.newTemp ()
  val T6 = Temp.newTemp ()
  val T7 = Temp.newTemp ()
  val T8 = Temp.newTemp ()
  val T9 = Temp.newTemp ()

  (* callee-saved registers *)
  val S0 = Temp.newTemp ()
  val S1 = Temp.newTemp ()
  val S2 = Temp.newTemp ()
  val S3 = Temp.newTemp ()
  val S4 = Temp.newTemp ()
  val S5 = Temp.newTemp ()
  val S6 = Temp.newTemp ()
  val S7 = Temp.newTemp ()

  (* stack pointer *)
  val SP = Temp.newTemp ()

  (* frame pointer; callee-saved *)
  val FP = Temp.newTemp ()

  (* return address; callee-saved *)
  val RA = Temp.newTemp ()
end

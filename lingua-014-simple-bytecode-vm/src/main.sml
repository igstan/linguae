structure Opcode =
  struct
    datatype t =
    | IADD
    | ISUB
    | IMUL
    | ILT
    | IEQ
    | BR
    | BRT
    | BRF
    | ICONST
    | LOAD
    | GLOAD
    | STORE
    | GSTORE
    | PRINT
    | POP
    | CALL
    | RET
    | HALT

    fun decode n =
      case n of
      | 1  => IADD
      | 2  => ISUB
      | 3  => IMUL
      | 4  => ILT
      | 5  => IEQ
      | 6  => BR
      | 7  => BRT
      | 8  => BRF
      | 9  => ICONST
      | 10 => LOAD
      | 11 => GLOAD
      | 12 => STORE
      | 13 => GSTORE
      | 14 => PRINT
      | 15 => POP
      | 16 => CALL
      | 17 => RET
      | 18 => HALT
      | _  => raise Fail ("unknown opcode: " ^ Int.toString n)

    fun encode b =
      case b of
      | IADD   => 1
      | ISUB   => 2
      | IMUL   => 3
      | ILT    => 4
      | IEQ    => 5
      | BR     => 6
      | BRT    => 7
      | BRF    => 8
      | ICONST => 9
      | LOAD   => 10
      | GLOAD  => 11
      | STORE  => 12
      | GSTORE => 13
      | PRINT  => 14
      | POP    => 15
      | CALL   => 16
      | RET    => 17
      | HALT   => 18

    fun arity opcode =
      case opcode of
      | (IADD | ISUB | IMUL | ILT | IEQ | PRINT | POP | RET | HALT) => 0
      | (BR | BRT | BRF | ICONST | LOAD | GLOAD | STORE | GSTORE)   => 1
      | (CALL)                                                      => 2

    fun name opcode =
      case opcode of
      | IADD   => "iadd"
      | ISUB   => "isub"
      | IMUL   => "imul"
      | ILT    => "ilt"
      | IEQ    => "ieq"
      | BR     => "br"
      | BRT    => "brt"
      | BRF    => "brf"
      | ICONST => "iconst"
      | LOAD   => "load"
      | GLOAD  => "gload"
      | STORE  => "store"
      | GSTORE => "gstore"
      | PRINT  => "print"
      | POP    => "pop"
      | CALL   => "call"
      | RET    => "ret"
      | HALT   => "halt"
  end

structure VM =
  struct
    open Fn.Syntax infix |>

    fun run code {
      startAddr,
      stackSize,
      heapSize,
      trace = {
        heap = traceHeap,
        stack = traceStack,
        registers = traceRegs
      }
    } =
      let
        open Opcode

        val defaultHeapSize = 1024
        val defaultStackSize = 1024

        val heap = Array.array (Option.getOpt (heapSize, defaultHeapSize), 0)
        val stack = Array.array (Option.getOpt (stackSize, defaultStackSize), 0)

        val sp = ref ~1      (* stack pointer *)
        val fp = ref 0       (* frame pointer *)
        val ip = ref startAddr (* instruction pointer *)

        fun log message value =
          value before Console.println (message ^ Int.toString value)

        fun push v = Array.update (stack, Ref.incrementAndGet sp(* |> log "sp: " *), v)
        fun pop () = Array.sub (stack, Ref.getAndDecrement sp)
        fun pop2 () =
          let
            val b = Array.sub (stack, Ref.getAndDecrement sp)
            val a = Array.sub (stack, Ref.getAndDecrement sp)
          in
            (a, b)
          end

        fun fetch () = ignore (Ref.incrementAndGet ip)

        fun decode () = Opcode.decode (Array.sub (code, !ip))

        fun dumpStack tracePrefixLength =
          if not traceStack then tracePrefixLength else
            let
              val m = Int.clamp { min=0, max=20 } (20 - tracePrefixLength)

              val elems =
                case !sp of
                | ~1 => []
                | n => List.take (Array.toList stack, n + 1)

              val dump =
                String.repeat m " " ^ "│ stack: " ^ Show.list Show.int elems
            in
              print dump;
              tracePrefixLength + String.size dump
            end

        fun dumpHeap tracePrefixLength =
          if not traceHeap then tracePrefixLength else
            let
              val m = Int.clamp { min=0, max=70 } (70 - tracePrefixLength)
              val elems = Array.toList heap
              val dump = String.repeat m " " ^ "│ heap: " ^ Show.list Show.int elems
            in
              print dump;
              tracePrefixLength + String.size dump
            end

        fun dumpRegisters tracePrefixLength =
          if not traceRegs then "" else
            let
              val m = Int.clamp { min=0, max=90 } (90 - tracePrefixLength)
              val r = [
                ("sp", !sp),
                ("fp", !fp),
                ("ip", !ip)
              ] |> List.map (fn (name, value) => name ^ ": " ^ StringCvt.padLeft #" " 2 (Show.int value))
                |> String.concatWith "; "
              val dump = String.repeat m " " ^ "│ " ^ r
            in
              dump
            end

        fun trace opcode =
          let
            fun arg n = Int.toString (Array.sub (code, !ip + n))
            val addr = StringCvt.padLeft #"0" 4 (Int.fmt StringCvt.DEC (!ip))
            val name = addr ^ ": " ^ StringCvt.padRight #" " 6 (Opcode.name opcode)
            val dump =
              case Opcode.arity opcode of
              | 0 => name
              | 1 => name ^ " " ^ arg 1
              | 2 => name ^ " " ^ arg 1 ^ " " ^ arg 2
              | n => raise Fail ("unknown arity: " ^ Show.int n)
          in
            print dump;
            String.size dump
          end

        val vmStdout = ref []
        (*) val opcode = ref (decode ())
        val continue = ref true
      in
        while !continue do
          let
            val opcode = decode ();
            val tracePrefixLength = trace opcode
          in
            (*) fetch ();
            case opcode of
            | IADD   => push (pop () + pop ())
            | IMUL   => push (pop () * pop ())
            | ISUB   => let val (a, b) = pop2 () in push (a - b) end
            | ILT    => let val (a, b) = pop2 () in push (if a < b then 1 else 0) end
            | IEQ    => let val (a, b) = pop2 () in push (if a = b then 1 else 0) end
            | BR     => ip := Array.sub (code, Ref.incrementAndGet ip) - 1
            | BRT    => let val target = Ref.incrementAndGet ip in if pop () = 1 then ip := Array.sub (code, target) - 1 else () end
            | BRF    => let val target = Ref.incrementAndGet ip in if pop () = 0 then ip := Array.sub (code, target) - 1 else () end
            | ICONST => push (Array.sub (code, Ref.incrementAndGet ip))
            | STORE  => raise Fail "not implemented"
            | LOAD   => push (Array.sub (stack, !fp + Array.sub (code, Ref.incrementAndGet ip)))
            | GSTORE => Array.update (heap, Array.sub (code, Ref.incrementAndGet ip), pop ())
            | GLOAD  => push (Array.sub (heap, Array.sub (code, Ref.incrementAndGet ip)))
            | PRINT  => Ref.modify (fn a => Int.toString (pop ()) :: a) vmStdout
            | POP    => (ignore o pop) ()
            | HALT   => continue := false

            | RET => let
                val returns = !fp <> !sp
                val rvalue = if returns then pop () else 0
              in
                sp := !fp;              (* pop the current activation frame *)
                ip := pop ();           (* pop the return address           *)
                fp := pop ();           (* restore previous frame pointer   *)
                sp := !sp - 1 - pop (); (* pop the function call arguments  *)
                if returns              (* push back the return value       *)
                then push rvalue
                else ()
              end

            | CALL => let
                val funAddr = Array.sub (code, Ref.incrementAndGet ip)
                val numArgs = Array.sub (code, Ref.incrementAndGet ip)
              in
                push numArgs;
                push (!fp);
                push (!ip); (* return address *)
                fp := !sp;
                ip := funAddr - 1
              end;

            tracePrefixLength
              |> dumpStack
              |> dumpHeap
              |> dumpRegisters
              |> Console.println;

            if (!ip + 1) < Array.length code
            then ()
            else continue := false;
            fetch ()
          end;
        (Console.println o String.concatWith "\n" o List.rev) (!vmStdout)
      end
  end

structure Main =
  struct
    val $ = Opcode.encode

    fun factorial n = Array.fromList let open Opcode in [
      $ LOAD, ~3,
      $ ICONST, 2,
      $ ILT,
      $ BRF, 10,
      $ ICONST, 1,
      $ RET,

      $ LOAD, ~3,
      $ LOAD, ~3,
      $ ICONST, 1,
      $ ISUB,
      $ CALL, 0, 1,
      $ IMUL,
      $ RET,

      $ ICONST, n,
      $ CALL, 0, 1,
      $ PRINT,
      $ HALT
    ] end

    val program = Array.fromList let open Opcode in [
      (*) fun helper
      $ ICONST, 3,
      $ ICONST, 4,
      $ IADD,
      $ GSTORE, 0,
      $ GLOAD, 0,
      $ PRINT,
      $ ICONST, 3,
      $ ICONST, 2,
      $ IADD,
      $ RET,

      (*) fun main
      $ ICONST, 1,
      $ ICONST, 2,
      $ IADD,
      $ CALL, 0, 1, (* helper *)
      $ PRINT,
      $ ICONST, 3,
      $ ICONST, 2,
      $ IADD,
      $ PRINT
    ] end

    fun main () =
      VM.run (factorial 3) {
        startAddr = 22,
        stackSize = NONE,
        heapSize = SOME 10,
        trace = {
          stack = true,
          heap = false,
          registers = true
        }
      }
  end

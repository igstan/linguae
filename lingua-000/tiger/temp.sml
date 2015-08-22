structure Temp : TEMP =
struct
  structure F = Format

  structure Table = IntMapTable(
    type key = int
    fun getInt n = n
  )

  type temp = int

  val temps = ref 100

  fun newTemp () =
    Ref.getAndIncrement temps

  fun makeString t = "t" ^ Int.toString t

  type label = Symbol.symbol

  val labs = ref 0

  fun newLabel () =
    Symbol.symbol (F.format "L%d" [F.INT (Ref.getAndIncrement labs)])

  fun namedLabel name =
    Symbol.symbol name
end

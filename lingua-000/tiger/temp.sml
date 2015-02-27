structure Temp : TEMP =
struct
  type temp = int
  val temps = ref 100
  fun newTemp () =
    let
      val t = !temps
    in
      temps := t + 1;
      t
    end

  structure Table = IntMapTable(
    type key = int
    fun getInt n = n
  )

  fun makeString t = "t" ^ Int.toString t

  type label = Symbol.symbol

  local
    structure F = Format
    fun postinc x =
      let
        val i = !x
      in
        x := i + 1;
        i
      end
    val labs = ref 0
  in
    fun newLabel () = Symbol.symbol (F.format "L%d" [F.INT (postinc labs)])
    val namedLabel = Symbol.symbol
  end
end

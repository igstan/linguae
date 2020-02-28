structure Show : SHOW =
struct
  type 'a show = 'a -> string

  fun arrayToList arr = Array.foldr op:: [] arr
  fun vectorToList vec = Vector.foldr op:: [] vec
  fun comma values = concat (List.interleave ", " values)
  fun between a b c = a ^ (comma c) ^ b
  fun tuple values = between "(" ")" values

  fun println s = print (s ^ "\n")

  val int = Int.toString
  val bool = Bool.toString
  val char = Char.toString
  val real = Real.toString
  val word = Word.toString
  val unit = fn _ => "()"
  val string = fn s => "\"" ^ s ^ "\""

  fun list show xs =
    between "[" "]" (map show xs)

  fun array show xs =
    between "[|" "|]" (map show (arrayToList xs))

  fun vector show xs =
    between "#[" "]" (map show (vectorToList xs))

  fun option show a =
    case a of
      NONE => "NONE"
    | SOME x => "SOME " ^ show x

  fun tuple2 (sa, sb) (a, b) =
    tuple [sa a, sb b]

  fun tuple3 (sa, sb, sc) (a, b, c) =
    tuple [sa a, sb b, sc c]

  fun tuple4 (sa, sb, sc, sd) (a, b, c, d) =
    tuple [sa a, sb b, sc c, sd d]

  fun tuple5 (sa, sb, sc, sd, se) (a, b, c, d, e) =
    tuple [sa a, sb b, sc c, sd d, se e]

  fun tuple6 (sa, sb, sc, sd, se, sf) (a, b, c, d, e, f) =
    tuple [sa a, sb b, sc c, sd d, se e, sf f]

  fun tuple7 (sa, sb, sc, sd, se, sf, sg) (a, b, c, d, e, f, g) =
    tuple [sa a, sb b, sc c, sd d, se e, sf f, sg g]
end

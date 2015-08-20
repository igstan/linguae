signature LIST_PAIR =
sig
  include LIST_PAIR

  val zipOption : 'a list * 'b list -> ('a option * 'b option) list
  val zipWithIndex : 'a list -> ('a * int) list
end

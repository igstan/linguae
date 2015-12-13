signature TEMP =
sig
  eqtype temp
  val newTemp : unit -> temp
  val makeString : temp -> string

  type label = Symbol.symbol
  val newLabel : unit -> label
  val namedLabel : string -> label
end

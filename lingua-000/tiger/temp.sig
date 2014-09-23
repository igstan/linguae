signature TEMP =
sig
  eqtype temp
  structure Table : TABLE sharing type Table.key = temp
  val newTemp : unit -> temp
  val makeString : temp -> string

  type label = Symbol.symbol
  val newLabel : unit -> label
  val namedLabel : string -> label
end

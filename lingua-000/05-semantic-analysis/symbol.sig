(*
 * An abstraction for symbol tables.
 *)
signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string

  type 'a table
  val empty : 'a table
  val set : 'a table -> symbol -> 'a -> 'a table
  val get : 'a table -> symbol -> 'a option
end

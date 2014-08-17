signature SHOW =
sig
  type field
  val record : string -> field list -> string
  val field : string -> string -> field
end

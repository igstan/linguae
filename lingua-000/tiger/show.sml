structure Show : Show =
struct
  type field = string
  val record : string -> field list -> string
  val field : string -> string -> field
end

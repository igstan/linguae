structure Types =
struct
  (*
   * Each array or record declaration introduces a new type, regardless of
   * the structure. The `unique` type is used to encode that by exploiting
   * the fact that, in SML, no two `ref` instances are equal.
   *)
  type unique = unit ref

  (*
   * Supported Data Types
   *)
  datatype ty =
    NIL
  | INT
  | UNIT
  | STRING
  | NAME of Symbol.symbol * ty option ref
  | ARRAY of ty * unique
  | RECORD of (Symbol.symbol * ty) list * unique

end

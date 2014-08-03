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

  fun areEqual types =
    case types of
      (NIL, NIL) => true
    | (NIL, RECORD(_, _)) => true
    | (RECORD(_, _), NIL) => true
    | (INT, INT) => true
    | (UNIT, UNIT) => true
    | (STRING, STRING) => true
    | (NAME(name1, ty1), NAME(name2, ty2)) =>
        if (Symbol.name name1) <> (Symbol.name name2) then
          false
        else
          areEqual (Option.valOf (!ty1), Option.valOf (!ty2))
    | (ARRAY(_, uniq1), ARRAY(_, uniq2)) => uniq1 = uniq2
    | (RECORD(_, uniq1), RECORD(_, uniq2)) => uniq1 = uniq2
    | _ => false

end

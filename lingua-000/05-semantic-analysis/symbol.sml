structure Symbol :> SYMBOL =
struct

  exception Symbol

  type symbol = string * int

  val nextsym = ref 0

  val hashtable : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (128, Symbol)

  fun symbol name =
    case HashTable.find hashtable name of
      SOME i => (name, i)
    | NONE =>
        let
          val i = !nextsym
        in
          nextsym := i + 1;
          HashTable.insert hashtable (name, i);
          (name, i)
        end

  fun name (s, _) = s

  type 'a table = 'a IntBinaryMap.map

  val empty = IntBinaryMap.empty
  fun set table (_, key) value = IntBinaryMap.insert (table, key, value)
  fun get table (_, key) = IntBinaryMap.find (table, key)

end

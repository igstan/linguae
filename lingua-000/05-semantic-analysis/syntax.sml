structure Syntax : SYNTAX =
struct
  local
    fun join xs sep =
      let
        val folder = fn (x, acc) => acc ^ sep ^ x
      in
        if List.null xs then ""
        else (List.hd xs) ^ (List.foldl (folder) "" (List.tl xs))
      end
  in
    fun showOper oper =
      let open Ast in
        case oper of
          PlusOp => "+"
        | MinusOp => "-"
        | TimesOp => "*"
        | DivideOp => "/"
        | EqOp => "="
        | NeqOp => "<>"
        | LtOp => "<"
        | LeOp => "<="
        | GtOp => ">"
        | GeOp => ">="
      end

      fun showType t =
        let open Types in
          case t of
            NIL => "nil"
          | INT => "int"
          | UNIT => "unit"
          | STRING => "string"
          | NAME(name, _) => Symbol.name name
          | ARRAY(ty, _) => "array of " ^ (showType ty)
          | RECORD(fields, _) => "{ "^ (join (List.map (fn (sym, ty) => (Symbol.name sym) ^" : "^ (showType ty)) fields) ", ") ^" }"
        end
  end
end

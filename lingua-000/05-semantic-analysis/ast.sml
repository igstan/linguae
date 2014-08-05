structure Ast =
struct
  type pos = int
  and symbol = Symbol.symbol

  datatype var =
      SimpleVar of symbol * pos       (* l-value      *)
    | FieldVar of var * symbol * pos  (* l-value.id   *)
    | SubscriptVar of var * exp * pos (* l-value[exp] *)

  and exp =
      VarExp of var
    | NilExp
    | IntExp of int
    | StringExp of string * pos
    | CallExp of { func: symbol, args: exp list, pos: pos }
    | OpExp of { left: exp, oper: oper, right: exp, pos: pos }
    | RecordExp of { fields: (symbol * exp * pos) list, name: symbol, pos: pos }
    | SeqExp of (exp * pos) list
    | AssignExp of { var: var, exp: exp, pos: pos }
    | IfExp of { test: exp, then': exp, else': exp option, pos: pos }
    | WhileExp of { test: exp, body: exp, pos: pos }
    | ForExp of { var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp, pos: pos }
    | BreakExp of pos
    | LetExp of { decs: dec list, body: exp, pos: pos }
    | ArrayExp of { typ: symbol, size: exp, init: exp, pos: pos }

  and dec =
      TypeDec of { name: symbol, ty: ty, pos: pos } list
    | FunctionDec of fundec list
    | VarDec of {
        name: symbol,
        escape: bool ref,
        typ: (symbol * pos) option,
        init: exp,
        pos: pos
      }

  and ty =
      NameTy of symbol * pos
    | RecordTy of field list
    | ArrayTy of symbol * pos

  and oper =
      PlusOp
    | MinusOp
    | TimesOp
    | DivideOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp

  and field = Field of {
        name: symbol,
        escape: bool ref,
        typ: symbol,
        pos: pos
      }

  and fundec = FunDec of {
        name: symbol,
        params: field list,
        result: (symbol * pos) option,
        body: exp,
        pos: pos
      }

  fun isComparisonOperator oper =
    case oper of
      EqOp => true
    | NeqOp => true
    | LtOp => true
    | LeOp => true
    | GtOp => true
    | GeOp => true
    | _ => false
end

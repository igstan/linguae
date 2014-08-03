structure ShowAst : SHOW_AST =
struct
  open Ast

  fun join xs sep =
    let
      val folder = fn (x, acc) => acc ^ sep ^ x
    in
      if List.null xs then ""
      else (List.hd xs) ^ (List.foldl (folder) "" (List.tl xs))
    end

  fun field name value = (name, value)

  val record : string -> (string * string) list -> string =
    fn name => fn fields =>
      name ^" { "^ (join (List.map (fn (a,b) => a ^"="^ b) fields) ", ") ^" } "

  fun showOper oper =
    case oper of
      PlusOp => "PlusOp"
    | MinusOp => "MinusOp"
    | TimesOp => "TimesOp"
    | DivideOp => "DivideOp"
    | EqOp => "EqOp"
    | NeqOp => "NeqOp"
    | LtOp => "LtOp"
    | LeOp => "LeOp"
    | GtOp => "GtOp"
    | GeOp => "GeOp"

  and showVar var =
    case var of
      SimpleVar(symbol, pos) => "SimpleVar("^ (Symbol.name symbol) ^", "^ (Int.toString pos) ^")"
    | FieldVar(var, symbol, pos) => "FieldVar("^ (showVar var) ^", "^ (Symbol.name symbol) ^", "^ (Int.toString pos) ^")"
    | SubscriptVar(var, exp, pos) => "SubscriptVar("^ (showVar var) ^", "^ (showExp exp) ^", "^ (Int.toString pos) ^")"

  and showList xs =
    let
      val acc = ""
      val folder = fn (x, acc) => acc ^","^ x
    in
      "[" ^ (List.foldl folder acc xs) ^ "]"
    end

  and showExp exp =
    case exp of
      VarExp(v) => "VarExp("^ showVar(v) ^")"
    | NilExp => "NilExp"
    | IntExp(n) => "IntExp("^ (Int.toString n) ^")"
    | StringExp(s, pos) =>
        "StringExp("^ s ^", "^ (Int.toString pos) ^")"
    | CallExp { func, args, pos } =>
        record "CallExp" [
          field "func" (Symbol.name func),
          field "args" (showList (List.map showExp args)),
          field "pos" (Int.toString pos)
        ]
    | OpExp { left, oper, right, pos } =>
        record "OpExp" [
          field "left" (showExp left),
          field "oper" (showOper oper),
          field "right" (showExp right),
          field "pos" (Int.toString pos)
        ]
    | RecordExp { fields, name, pos } =>
      let
        fun showField (symbol, exp, pos) =
          "("^ (Symbol.name symbol) ^", "^ (showExp exp) ^", "^ (Int.toString pos) ^")"
      in
        record "RecordExp" [
          field "fields" (showList (List.map showField fields)),
          field "name" (Symbol.name name),
          field "pos" (Int.toString pos)
        ]
      end
    | SeqExp(exprs) =>
        "SeqExp("^ showList (List.map showExp (List.map #1 exprs)) ^")"
    | AssignExp { var, exp, pos } =>
        record "AssignExp" [
          field "var" (showVar var),
          field "exp" (showExp exp),
          field "pos" (Int.toString pos)
        ]
    | IfExp { test, then', else', pos } =>
        let
          val els = Option.getOpt ((Option.map (fn e => "SOME "^ showExp e) else'), "NONE")
        in
          record "IfExp" [
            field "test" (showExp test),
            field "then'" (showExp then'),
            field "else'" els,
            field "pos" (Int.toString pos)
          ]
        end
    | WhileExp { test, body, pos } =>
        record "WhileExp" [
          field "test" (showExp test),
          field "body" (showExp body),
          field "pos" (Int.toString pos)
        ]
    | ForExp { var, escape, lo, hi, body, pos } =>
        record "ForExp" [
          field "var" (Symbol.name var),
          field "escape" (Bool.toString (!escape)),
          field "lo" (showExp lo),
          field "hi" (showExp hi),
          field "body" (showExp body),
          field "pos" (Int.toString pos)
        ]
    | BreakExp(pos) =>
      "BreakExp("^ (Int.toString pos) ^")"
    | LetExp { decs, body, pos } =>
      record "LetExp" [
        field "decs" (showList (List.map showDec decs)),
        field "body" (showExp body),
        field "pos" (Int.toString pos)
      ]
    | ArrayExp { typ: symbol, size: exp, init: exp, pos: pos } =>
      record "ArrayExp" [
        field "typ" (Symbol.name typ),
        field "size" (showExp size),
        field "init" (showExp init),
        field "pos" (Int.toString pos)
      ]

  and showDec dec =
    case dec of
      TypeDec(decs) =>
        let
          fun showSingleDec { name, ty, pos } =
            "{ name="^ Symbol.name name ^", ty="^ showTy ty ^", pos="^ (Int.toString pos) ^" }"
        in
          "TypeDec("^ showList (List.map showSingleDec decs) ^")"
        end
    | FunctionDec(fundecs) => showList (List.map showFundec fundecs)
    | VarDec { name, escape, typ, init, pos } =>
        let
          fun showTyp typ = Option.getOpt (Option.map (fn t : symbol * pos => "SOME"^ Symbol.name (#1 t)) typ, "NONE")
        in
          record "VarDec" [
            field "name" (Symbol.name name),
            field "escape" (Bool.toString (!escape)),
            field "typ" (showTyp typ),
            field "init" (showExp init),
            field "pos" (Int.toString pos)
          ]
        end

  and showField f =
    case f of
      Field { name, escape, typ, pos } =>
        record "Field" [
          field "name" (Symbol.name name),
          field "escape" (Bool.toString (!escape)),
          field "typ" (Symbol.name typ),
          field "pos" (Int.toString pos)
        ]

  and showFundec fundec =
    case fundec of
      FunDec { name, params, result, body, pos } =>
        let
          val res = Option.getOpt (Option.map (fn r => "SOME"^ Symbol.name (#1 r)) result, "NONE")
        in
          record "FunDec" [
            field "name" (Symbol.name name),
            field "params" (showList (List.map showField params)),
            field "result" res,
            field "body" (showExp body),
            field "pos" (Int.toString pos)
          ]
        end

  and showTy ty =
    case ty of
      NameTy(name, pos) => "NameTy("^ Symbol.name name ^", pos="^ (Int.toString pos) ^" }"
    | RecordTy(fields) => "RecordTy("^ showList (List.map showField fields) ^")"
    | ArrayTy(name, pos) => "ArrayTy("^ Symbol.name name ^", pos="^ (Int.toString pos) ^" }"
end

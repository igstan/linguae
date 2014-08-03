structure Semant =
struct
  open Lang

  exception TypeError of Ast.pos * string

  fun error pos msg = raise TypeError (pos, msg)

  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table

  type expty = { exp: Translate.exp, ty: Types.ty }

  val dummyExpty = { exp = (), ty = Types.NIL }

  val translateVar : venv -> tenv -> Ast.var -> expty =
    fn venv => fn tenv => fn ast => dummyExpty

  fun translateExp venv tenv ast =
    let
      fun typecheckVarExp venv tenv var =
        case var of
          Ast.SimpleVar(id, pos) =>
            (case Symbol.get venv id of
              SOME(Env.VarEntry { ty }) => { exp = (), ty = ty }
            | SOME(Env.FunEntry { ... }) => error pos "impossible: a VarExp cannot contain a FunEntry"
            | NONE => error pos ("undefined variable: " ^ Symbol.name id))
          | Ast.FieldVar(var, id, pos) => error pos "TODO"
          | Ast.SubscriptVar(var, exp, pos) => error pos "TODO"

      fun equalTypes types =
        let
          open Types
        in
          case types of
            (NIL, NIL) => true
          | (NIL, RECORD(_, _)) => true (* page 113: "In Tiger, the expression `nil` belongs to any record type." *)
          | (RECORD(_, _), NIL) => true
          | (INT, INT) => true
          | (UNIT, UNIT) => true
          | (STRING, STRING) => true
          | (NAME(name1, ty1), NAME(name2, ty2)) =>
              if not ((Symbol.name name1) = (Symbol.name name2)) then
                false
              else
                equalTypes (Option.valOf (!ty1), Option.valOf (!ty2))
          | (ARRAY(_, uniq1), ARRAY(_, uniq2)) => uniq1 = uniq2
          | (RECORD(_, uniq1), RECORD(_, uniq2)) => uniq1 = uniq2
          | _ => false
        end

      fun typecheckCallExp func args pos =
        (*
         * 1. Obtain types of all arguments in `args`.
         * 2. Obtain `func` type signature.
         * 3. Match it with `args`.
         * 4. Return `func`'s return type.
         *)
        let
          val targs = List.map (fn arg => case translateExp venv tenv arg of { exp, ty } => ty) args (* throws for unbound identifiers *)
          val tfunc = Symbol.get venv func
        in
          case tfunc of
            SOME(Env.FunEntry { formals, result }) =>
              let
                val mismatched = List.filter (not <> equalTypes) (ListPair.zip (formals, targs))
                fun detail (formal, actual) =
                  "argument mismatch\n"
                ^ "  required: "^ (Syntax.showType actual) ^"\n"
                ^ "     found: "^ (Syntax.showType formal) ^"\n"
                val details = List.foldl op^ "" (List.map detail mismatched)
              in
                if List.null mismatched then
                  { exp = (), ty = result }
                else
                  error pos details
              end
          | SOME(Env.VarEntry { ... }) => error pos ("non-function in call position: "^ Symbol.name func)
          | NONE => error pos ("undefined variable: " ^ Symbol.name func)
        end

      fun typecheckOpExp left oper right pos =
        let
          open Ast
          val { exp = _, ty = tyleft } = translateExp venv tenv left
          val { exp = _, ty = tyright } = translateExp venv tenv right
        in
          case (tyleft, tyright) of
            (Types.NIL, Types.NIL) =>
              (case oper of
                (EqOp | NeqOp) => { exp = (), ty = Types.INT }
                | _ => error pos ("unit type not supported for operator: " ^ Syntax.showOper oper))
          | (Types.UNIT, Types.UNIT) =>
              (case oper of
                (EqOp | NeqOp) => { exp = (), ty = Types.INT }
                | _ => error pos ("unit type not supported for operator: " ^ Syntax.showOper oper))
          | (Types.INT, Types.INT) => { exp = (), ty = Types.INT }
          | (Types.STRING, Types.STRING) =>
              if isComparisonOperator oper then
                { exp = (), ty = Types.INT }
              else
                error pos ("string types not supported for operator: " ^ Syntax.showOper oper)
          | (Types.ARRAY(_, _), Types.ARRAY(_, _)) =>
              (case oper of
                (EqOp | NeqOp) => { exp = (), ty = Types.INT }
              | _ => error pos ("array types not supported for operator: " ^ Syntax.showOper oper))
          | (Types.RECORD(_, _), Types.RECORD(_, _)) =>
              (case oper of
                (EqOp | NeqOp) => { exp = (), ty = Types.INT }
              | _ => error pos ("record types not supported for operator: " ^ Syntax.showOper oper))
          | (type1, type2) =>
            error pos ("argument mismatch:\n"
                     ^ "             operator: "^ Syntax.showOper oper ^"\n"
                     ^ " 1st operand type was: "^ Syntax.showType type1 ^"\n"
                     ^ " 2nd operand type was: "^ Syntax.showType type2 ^"\n")
        end

      fun typecheckRecordExp fields name pos =
        let
          fun typecheckFields definitionFields uniq =
            let
              val typedFields = List.map (fn (symbol, exp, pos) => (symbol, #ty (translateExp venv tenv exp), pos)) fields

              val zipped = PairList.zipOption (definitionFields, typedFields)

              fun equalFields fields =
                case fields of
                  (SOME((name1, typ1)), SOME(name2, typ2, _)) =>
                    name1 = name2 andalso equalTypes (typ1, typ2)
                | (NONE, _) => false
                |(_, NONE) => false
              val mismatched = List.filter (not <> equalFields) zipped

              fun showDefined field =
                case field of
                  NONE => "n/a"
                | SOME((name, typ)) => Symbol.name name ^" : "^ Syntax.showType typ

              fun showUsed field =
                case field of
                  NONE => "n/a"
                | SOME((name, typ, pos)) => Symbol.name name ^" : "^ Syntax.showType typ ^" at line "^ Int.toString pos

              fun detail (defined, used) =
                  "  required: "^ (showDefined defined) ^"\n"
                ^ "     found: "^ (showUsed used) ^"\n"

              val details = List.foldl op^ "" (List.map detail mismatched)
            in
              if List.null mismatched then
                { exp = (), ty = Types.RECORD(definitionFields, uniq) }
              else
                error pos ("record structure mismatch\n"^ details)
            end
        in
          case Symbol.get tenv name of
            SOME(Types.RECORD(definitionFields, uniq)) => typecheckFields definitionFields uniq
          | SOME(typ) => error pos ("`"^ Symbol.name name ^"' is not a record")
          | NONE => error pos ("undefined record type: "^ Symbol.name name)
        end

      fun typecheckSeqExp exprs =
        let
          (*
           * We're using our hand-rolled version of `map` because we want to
           * remember the type of the last expression in the list, which will
           * become the type of the whole `SeqExp`.
           *)
          fun loop exprs result =
            case exprs of
              [] => result
            | (exp, pos) :: exprs => loop exprs (translateExp venv tenv exp)
        in
          loop exprs { exp = (), ty = Types.UNIT }
        end
    in
      case ast of
        Ast.VarExp(v) => typecheckVarExp venv tenv v
      | Ast.NilExp => { exp = (), ty = Types.NIL }
      | Ast.IntExp(i) => { exp = (), ty = Types.INT }
      | Ast.StringExp(s, pos) => { exp = (), ty = Types.STRING }
      | Ast.CallExp { func, args, pos } => typecheckCallExp func args pos
      | Ast.OpExp { left, oper, right, pos } => typecheckOpExp left oper right pos
      | Ast.RecordExp { fields, name, pos } => typecheckRecordExp fields name pos
      | Ast.SeqExp(exprs) => typecheckSeqExp exprs
      | Ast.AssignExp { var, exp, pos } => dummyExpty
      | Ast.IfExp { test, then', else', pos } => dummyExpty
      | Ast.WhileExp { test, body, pos } => dummyExpty
      | Ast.ForExp { var, escape, lo, hi, body, pos } => dummyExpty
      | Ast.BreakExp(pos) => dummyExpty
      | Ast.LetExp { decs, body, pos } => dummyExpty
      | Ast.ArrayExp { typ, size, init, pos } => dummyExpty
    end

  val translateDec : venv -> tenv -> Ast.dec -> { venv: venv, tenv: tenv } =
    fn venv => fn tenv => fn dec =>
      { venv = Symbol.empty, tenv = Symbol.empty }

  val translateTy : tenv -> Ast.ty -> Types.ty =
    fn tenv => fn typ => Types.NIL

  fun translateProgram ast =
    (
      (
        (translateExp Env.base_venv Env.base_tenv ast);
        ()
      ) handle
        TypeError(pos, message) => print ("type error [line "^ (Int.toString pos) ^"]: "^ message ^"\n")
    )
end

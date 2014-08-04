structure Semant =
struct
  open Lang

  exception TypeError of Ast.pos * string

  fun error pos msg = raise TypeError (pos, msg)

  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table

  type expty = { exp: Translate.exp, ty: Types.ty }

  val dummyExpty = { exp = (), ty = Types.NIL }

  fun translateVar venv tenv var =
    let
      fun simpleVar name pos =
        case Symbol.get venv name of
          SOME(Env.VarEntry { ty }) => { exp = (), ty = ty }
        | SOME(Env.FunEntry { ... }) => error pos ("undefined variable: " ^ Symbol.name name)
        | NONE => error pos ("undefined variable: " ^ Symbol.name name)

      fun fieldVar var name pos =
        let
          val { exp, ty = tvar } = translateVar venv tenv var
          fun typecheckRecord fields =
            let
              val field = List.find (fn (sym, ty) => sym = name) fields
            in
              case field of
                SOME((_, ty)) => { exp = (), ty = ty }
              | NONE => error pos ("no such field on record\n"
                                 ^ "   field: "^ Symbol.name name ^"\n"
                                 ^ "  record: "^ Syntax.showType tvar ^"\n")
            end
        in
          case tvar of
            Types.RECORD(fields, _) => typecheckRecord fields
          | t => error pos ("field access on non-record type\n"
                          ^ "  actual type: " ^ Syntax.showType t ^"\n"
                          ^ "        field: " ^ Symbol.name name ^"\n")
        end

      fun subscriptVar var exp pos =
        let
          val { exp = _, ty = tvar } = translateVar venv tenv var
          val { exp = _, ty = texp } = translateExp venv tenv exp
          fun typecheckArray tarray =
            case texp of
              Types.INT => { exp = (), ty = tarray }
            | _ => error pos ("subscript index type mismatch\n"
                            ^ "  required: " ^ Syntax.showType Types.INT ^"\n"
                            ^ "     found: " ^ Syntax.showType texp ^"\n")
        in
          case tvar of
            Types.ARRAY(tarray, _) => typecheckArray tarray
          | t =>
              (* TODO: typecheck index type here, too. *)
              error pos ("subscript access on non-array type\n"
                       ^ "  actual type: " ^ Syntax.showType t ^"\n")
        end
    in
      case var of
        Ast.SimpleVar(name, pos) => simpleVar name pos
      | Ast.FieldVar(var, name, pos) => fieldVar var name pos
      | Ast.SubscriptVar(var, exp, pos) => subscriptVar var exp pos
    end

  and translateExp venv tenv ast =
    let
      fun typecheckCallExp func args pos =
        (*
         * 1. Obtain types of all arguments in `args`.
         * 2. Obtain `func` type signature.
         * 3. Match it with `args`.
         * 4. Return `func`'s return type.
         *)
        let
          fun translateArg arg = #ty (translateExp venv tenv arg)
          val targs = List.map translateArg args (* throws for unbound identifiers *)
          val tfunc = Symbol.get venv func
        in
          case tfunc of
            SOME(Env.FunEntry { formals, result }) =>
              let
                val mismatched = List.filter (not >< Types.areEqual) (ListPair.zip (formals, targs))
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
              fun mapper (symbol, exp, pos) = (symbol, #ty (translateExp venv tenv exp), pos)
              val typedFields = List.map mapper fields
              val zipped = PairList.zipOption (definitionFields, typedFields)

              fun equalFields fields =
                case fields of
                  (SOME((name1, typ1)), SOME(name2, typ2, _)) =>
                    name1 = name2 andalso Types.areEqual (typ1, typ2)
                | (NONE, _) => false
                |(_, NONE) => false
              val mismatched = List.filter (not >< equalFields) zipped

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

      fun typecheckAssignExp var exp pos =
        let
          val { exp = _, ty = tvar } = translateVar venv tenv var
          val { exp = _, ty = texp } = translateExp venv tenv exp
        in
          if Types.areEqual (tvar, texp) then
            { exp = (), ty = Types.UNIT }
          else
            error pos ("type mismatch in assignment\n"
                     ^ "  required: "^ Syntax.showType tvar ^"\n"
                     ^ "     found: "^ Syntax.showType texp ^"\n")
        end

      fun typecheckIfExp tst thn els pos =
        let
          val { exp = _, ty = ttst } = translateExp venv tenv tst
          val { exp = _, ty = tthn } = translateExp venv tenv thn
          fun typecheckBranches _ =
            case els of
              NONE => { exp = (), ty = tthn }
            | SOME(els) =>
              let
                val { exp = _, ty = tels } = translateExp venv tenv els
              in
                if Types.areEqual (tthn, tels) then
                  { exp = (), ty = tthn }
                else
                  error pos ("branch types in if expression don't match up\n"
                           ^ "  then type: "^ Syntax.showType tthn ^"\n"
                           ^ "  else type: "^ Syntax.showType tels ^"\n")
              end
        in
          case ttst of
            Types.INT => typecheckBranches ()
          | t => error pos ("type mismatch in if condition\n"
                          ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                          ^ "     found: "^ Syntax.showType ttst ^"\n")
        end

      fun typecheckWhileExp test body pos =
        let
          val { exp = _, ty = ttest } = translateExp venv tenv test
        in
          case ttest of
            Types.INT => { exp = (), ty = Types.UNIT }
          | t => error pos ("type mismatch in while condition\n"
                          ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                          ^ "     found: "^ Syntax.showType ttest ^"\n")
        end

      fun typecheckForExp var escape lo hi body pos =
        let
          val { exp = _, ty = tlo } = translateExp venv tenv lo
          val { exp = _, ty = thi } = translateExp venv tenv hi
          (*
           * Typecheck the body in a value environment containing the loop
           * variable.
           *)
          val bodyVenv = Symbol.set venv var (Env.VarEntry { ty = Types.INT })
          (*
           * We're not interested in the result, just in potential typechecking
           * exceptions.
           *)
          val _ = translateExp bodyVenv tenv body
        in
          case (tlo, thi) of
            (Types.INT, Types.INT) => { exp = (), ty = Types.UNIT }
          | (t, Types.INT) =>
              error pos ("for expression type mismatch\n"
                       ^ "low bound\n"
                       ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                       ^ "     found: "^ Syntax.showType t ^"\n")
          | (Types.INT, t) =>
              error pos ("for expression type mismatch\n"
                       ^ "high bound\n"
                       ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                       ^ "     found: "^ Syntax.showType t ^"\n")
          | (t1, t2) =>
              error pos ("for expression type mismatch\n"
                       ^ "low bound\n"
                       ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                       ^ "     found: "^ Syntax.showType t1 ^"\n"
                       ^ "high bound\n"
                       ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                       ^ "     found: "^ Syntax.showType t2 ^"\n")
        end
    in
      case ast of
        Ast.VarExp(var) => translateVar venv tenv var
      | Ast.NilExp => { exp = (), ty = Types.NIL }
      | Ast.IntExp(i) => { exp = (), ty = Types.INT }
      | Ast.StringExp(s, pos) => { exp = (), ty = Types.STRING }
      | Ast.CallExp { func, args, pos } => typecheckCallExp func args pos
      | Ast.OpExp { left, oper, right, pos } => typecheckOpExp left oper right pos
      | Ast.RecordExp { fields, name, pos } => typecheckRecordExp fields name pos
      | Ast.SeqExp(exprs) => typecheckSeqExp exprs
      | Ast.AssignExp { var, exp, pos } => typecheckAssignExp var exp pos
      | Ast.IfExp { test, then', else', pos } => typecheckIfExp test then' else' pos
      | Ast.BreakExp(pos) => dummyExpty
      | Ast.LetExp { decs, body, pos } => dummyExpty
      | Ast.WhileExp { test, body, pos } => typecheckWhileExp test body pos
      | Ast.ForExp { var, escape, lo, hi, body, pos } => typecheckForExp var escape lo hi body pos
      | Ast.ArrayExp { typ, size, init, pos } => dummyExpty
    end

  val translateDec : venv -> tenv -> Ast.dec -> { venv: venv, tenv: tenv } =
    fn venv => fn tenv => fn dec =>
      { venv = Symbol.empty, tenv = Symbol.empty }

  val translateTy : tenv -> Ast.ty -> Types.ty =
    fn tenv => fn typ => Types.NIL

  fun translateProgram ast =
    let
      val { exp, ty } = translateExp Env.base_venv Env.base_tenv ast
    in
      print ("type: "^ (Syntax.showType ty) ^"\n")
    end
    handle
      TypeError(pos, message) =>
        print ("type error [line "^ (Int.toString pos) ^"]: "^ message ^"\n")
end

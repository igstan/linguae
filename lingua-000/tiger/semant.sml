structure Semant =
struct
  open Lang

  structure L = List

  exception TypeError of Ast.pos * string

  type venv = Env.enventry Symbol.table
  type tenv = Env.ty Symbol.table
  type expty = { exp: Translate.exp, ty: Types.ty }

  fun error pos msg = raise TypeError (pos, msg)

  fun translateVar venv tenv var { insideLoop } =
    let
      fun simpleVar name pos =
        case Symbol.get venv name of
          SOME(Env.VarEntry { ty }) => { exp = (), ty = Types.actual ty }
        | SOME(Env.FunEntry { ... }) => error pos ("undefined variable: " ^ Symbol.name name)
        | NONE => error pos ("undefined variable: " ^ Symbol.name name)

      fun fieldVar var name pos =
        let
          val { exp, ty = tvar } = translateVar venv tenv var { insideLoop = insideLoop }
          fun typecheckRecord fields =
            let
              val field = L.find (fn (sym, ty) => sym = name) fields
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
          val { exp = _, ty = tvar } = translateVar venv tenv var { insideLoop = insideLoop }
          val { exp = _, ty = texp } = translateExp venv tenv exp { insideLoop = insideLoop }
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

  and translateExp venv tenv ast ({ insideLoop }) =
    let
      fun typecheckCallExp func args pos =
        (*
         * 1. Obtain types of all arguments in `args`.
         * 2. Obtain `func` type signature.
         * 3. Match it with `args`.
         * 4. Return `func`'s return type.
         *)
        let
          fun translateArg arg = #ty (translateExp venv tenv arg { insideLoop = insideLoop })
          val targs = L.map translateArg args (* throws for unbound identifiers *)
          val tfunc = Symbol.get venv func
        in
          case tfunc of
            SOME(Env.FunEntry { formals, result }) =>
              let
                val mismatched = L.filter (not <|> Types.areEqual) (ListPair.zip (formals, targs))
                fun detail (formal, actual) =
                  "argument mismatch\n"
                ^ "  required: "^ (Syntax.showType actual) ^"\n"
                ^ "     found: "^ (Syntax.showType formal) ^"\n"
                val details = L.foldl op^ "" (L.map detail mismatched)
              in
                if L.null mismatched then
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
          val { exp = _, ty = tyleft } = translateExp venv tenv left { insideLoop = insideLoop }
          val { exp = _, ty = tyright } = translateExp venv tenv right { insideLoop = insideLoop }
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
              fun mapper (symbol, exp, pos) = (symbol, #ty (translateExp venv tenv exp { insideLoop = insideLoop }), pos)
              val typedFields = L.map mapper fields
              val zipped = PairList.zipOption (definitionFields, typedFields)

              fun equalFields fields =
                case fields of
                  (SOME((name1, typ1)), SOME(name2, typ2, _)) =>
                    name1 = name2 andalso Types.areEqual (typ1, typ2)
                | (NONE, _) => false
                |(_, NONE) => false
              val mismatched = L.filter (not <|> equalFields) zipped

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

              val details = L.foldl op^ "" (L.map detail mismatched)
            in
              if L.null mismatched then
                { exp = (), ty = Types.RECORD(definitionFields, uniq) }
              else
                error pos ("record structure mismatch\n"^ details)
            end
        in
          case Option.map Types.actual (Symbol.get tenv name) of
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
            | (exp, pos) :: exprs => loop exprs (translateExp venv tenv exp { insideLoop = insideLoop })
        in
          loop exprs { exp = (), ty = Types.UNIT }
        end

      fun typecheckAssignExp var exp pos =
        let
          val { exp = _, ty = tvar } = translateVar venv tenv var { insideLoop = insideLoop }
          val { exp = _, ty = texp } = translateExp venv tenv exp { insideLoop = insideLoop }
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
          val { exp = _, ty = ttst } = translateExp venv tenv tst { insideLoop = insideLoop }
          val { exp = _, ty = tthn } = translateExp venv tenv thn { insideLoop = insideLoop }
          fun typecheckBranches _ =
            case els of
              NONE => { exp = (), ty = tthn }
            | SOME(els) =>
              let
                val { exp = _, ty = tels } = translateExp venv tenv els { insideLoop = insideLoop }
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
          val { exp = _, ty = ttest } = translateExp venv tenv test { insideLoop = true }
        in
          case ttest of
            Types.INT => { exp = (), ty = Types.UNIT }
          | t => error pos ("type mismatch in while condition\n"
                          ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                          ^ "     found: "^ Syntax.showType ttest ^"\n")
        end

      fun typecheckForExp var escape lo hi body pos =
        let
          val { exp = _, ty = tlo } = translateExp venv tenv lo { insideLoop = insideLoop }
          val { exp = _, ty = thi } = translateExp venv tenv hi { insideLoop = insideLoop }
          (*
           * Typecheck the body in a value environment containing the loop
           * variable.
           *)
          val bodyVenv = Symbol.set venv var (Env.VarEntry { ty = Types.INT })
          (*
           * We're not interested in the result, just in potential typechecking
           * exceptions.
           *)
          val _ = translateExp bodyVenv tenv body { insideLoop = true }
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

      fun typecheckLetExp decs body pos =
        let
          val initEnvs = { venv = venv, tenv = tenv }
          fun folder (dec, { venv, tenv }) = translateDec venv tenv dec insideLoop
          val { venv = venv2, tenv = tenv2 } = L.foldl folder initEnvs decs
          val { exp = _, ty = tbody } = translateExp venv2 tenv2 body { insideLoop = insideLoop }
        in
          { exp = (), ty = tbody }
        end

      fun typecheckArrayExp typ size init pos =
        let
          val tarray = Option.map Types.actual (Symbol.get tenv typ)
          val { exp = _, ty = tsize } = translateExp venv tenv size { insideLoop = insideLoop }
          val { exp = _, ty = tinit } = translateExp venv tenv init { insideLoop = insideLoop }
          fun typecheckInit _ =
            case tarray of
              NONE => error pos ("type not found: "^ Symbol.name typ ^"\n")
            | SOME(Types.ARRAY(innerType, uniq)) =>
                if Types.areEqual (innerType, tinit) then
                  { exp = (), ty = Types.ARRAY(innerType, uniq) }
                else
                  error pos ("array init type mismatch\n"
                           ^ "  required: "^ Syntax.showType innerType ^"\n"
                           ^ "     found: "^ Syntax.showType tinit ^"\n")
            | SOME(other) =>
                error pos ("declared type is not an array: "^ Syntax.showType other ^"\n")
        in
          case tsize of
            Types.INT => typecheckInit ()
          | t => error pos ("array size type mismatch\n"
                          ^ "  required: "^ Syntax.showType Types.INT ^"\n"
                          ^ "     found: "^ Syntax.showType t ^"\n")
        end

      fun typecheckBreakExp pos =
        if insideLoop then
          { exp = (), ty = Types.UNIT }
        else
          error pos ("no outer while or for expression for this break statement")
    in
      case ast of
        Ast.VarExp(var) => translateVar venv tenv var { insideLoop = insideLoop }
      | Ast.NilExp => { exp = (), ty = Types.NIL }
      | Ast.IntExp(i) => { exp = (), ty = Types.INT }
      | Ast.StringExp(s, pos) => { exp = (), ty = Types.STRING }
      | Ast.CallExp { func, args, pos } => typecheckCallExp func args pos
      | Ast.OpExp { left, oper, right, pos } => typecheckOpExp left oper right pos
      | Ast.RecordExp { fields, name, pos } => typecheckRecordExp fields name pos
      | Ast.SeqExp(exprs) => typecheckSeqExp exprs
      | Ast.AssignExp { var, exp, pos } => typecheckAssignExp var exp pos
      | Ast.IfExp { test, then', else', pos } => typecheckIfExp test then' else' pos
      | Ast.WhileExp { test, body, pos } => typecheckWhileExp test body pos
      | Ast.ForExp { var, escape, lo, hi, body, pos } => typecheckForExp var escape lo hi body pos
      | Ast.BreakExp(pos) => typecheckBreakExp pos
      | Ast.LetExp { decs, body, pos } => typecheckLetExp decs body pos
      | Ast.ArrayExp { typ, size, init, pos } => typecheckArrayExp typ size init pos
    end

  and translateDec venv tenv dec insideLoop =
    case dec of
      Ast.TypeDec decs =>
        let
          (*
           * Gather type names and augment tenv with them. Don't typecheck
           * their definitions, though!
           *)
          fun augment ({ name, ty, pos }, tenv) = Symbol.set tenv name (Types.NAME(name, ref NONE))
          val augmentedTenv = L.foldl augment tenv decs

          fun processTypeDec { name, ty, pos } =
            let
              (* Verify type declarations using the augment tenv. *)
              val typ = translateTy augmentedTenv ty pos
              val header = Symbol.get augmentedTenv name
            in
              case header of
                (* Update refs in type names gathered in 1st pass. *)
                SOME(Types.NAME(_, tyRef)) => tyRef := SOME(typ)
              | SOME(other) => raise Fail("impossible: type must have been a NAME, but was: "^ Syntax.showType other)
              | NONE => raise Fail("impossible: type must have been found inside tenv: "^ Symbol.name name)
            end
        in
          L.map processTypeDec decs;
          { venv = venv, tenv = augmentedTenv }
        end

    | Ast.FunctionDec fundecs =>
      let
        val fundecsWithRecursiveFlag =
          let
            val freeVars = FreeVars.freeVarsInDec dec
            fun isRecursive (Ast.FunDec { name, ... }) = Symbol.has freeVars name
          in
            L.map (fn fundec => (fundec, isRecursive fundec)) fundecs
          end

        val recursiveWithoutReturnType =
          let
            fun hasNoReturnType (Ast.FunDec { result, ... }, isRecursive) =
              isRecursive andalso not (Option.isSome result)
          in
            L.filter hasNoReturnType fundecsWithRecursiveFlag
          end

        fun raiseRecursiveFunctionsNeedExplicitReturnType _ =
          let
            fun detail ((Ast.FunDec { name, pos, ... }, _), message) =
              message ^"  "^ Symbol.name name ^" at line "^ Int.toString pos ^"\n"
            val details = L.foldl detail "" recursiveWithoutReturnType
          in
            error 0 ("recursive functions need explicit return type\n"^ details)
          end

        fun augment ((Ast.FunDec { name, params, result, body, pos }, isRecursive), venv) =
          let
            fun paramMapper (Ast.Field { name, escape, typ, pos }) =
              case Symbol.get tenv typ of
                NONE => error pos ("type not found: "^ Symbol.name typ)
              | SOME(t) => (name, t)
            val typedParams = L.map paramMapper params
            val tformals = L.map #2 typedParams
            fun tresultMapper (returnType, returnPos) =
              case Symbol.get tenv returnType of
                NONE => error returnPos ("type not found: "^ Symbol.name returnType)
              | SOME(t) => t
            val tresult = Option.map tresultMapper result
          in
            if isRecursive then
              case tresult of
                NONE => raise Fail("impossible: type must have been present because this is a recursive function"^ Symbol.name name)
              | SOME(tresult) => Symbol.set venv name (Env.FunEntry {
                  formals = tformals,
                  result = tresult
                })
            else
              venv
          end

        fun funDecMapper ((Ast.FunDec { name, params, result, body, pos }, isRecursive), venv) =
          if isRecursive then
            let
              fun paramMapper (Ast.Field { name, escape, typ, pos }) =
                case Symbol.get tenv typ of
                  NONE => error pos ("type not found: "^ Symbol.name typ)
                | SOME(t) => (name, t)
              val typedParams = L.map paramMapper params
              fun addParam ((name, ty), venv) = Symbol.set venv name (Env.VarEntry { ty = ty })
              val bodyEnv = L.foldl addParam venv typedParams
            in
              (*
               * Nothing to record here, just typecheck the body. The function
               * signature is already in venv.
               *)
              translateExp bodyEnv tenv body { insideLoop = false };
              venv
            end
          else
            let
              fun paramMapper (Ast.Field { name, escape, typ, pos }) =
                case Symbol.get tenv typ of
                  NONE => error pos ("type not found: "^ Symbol.name typ)
                | SOME(t) => (name, t)
              val typedParams = L.map paramMapper params
              val tformals = L.map #2 typedParams
              fun tresultMapper (returnType, returnPos) =
                case Symbol.get tenv returnType of
                  NONE => error returnPos ("type not found: "^ Symbol.name returnType)
                | SOME(t) => t
              val tresult = Option.map tresultMapper result
              fun addParam ((name, ty), venv) = Symbol.set venv name (Env.VarEntry { ty = ty })
              val bodyEnv = L.foldl addParam venv typedParams
              val { exp = _, ty = tbody } = translateExp bodyEnv tenv body { insideLoop = false }
            in
              case tresult of
                NONE => Symbol.set venv name (Env.FunEntry { formals = tformals, result = tbody })
              | SOME(tresult) =>
                if Types.areEqual (tresult, tbody) then
                  Symbol.set venv name (Env.FunEntry { formals = tformals, result = tbody })
                else
                  error pos ("function declaration type mismatch\n"
                           ^ "  required: "^ Syntax.showType tresult ^"\n"
                           ^ "     found: "^ Syntax.showType tbody ^"\n")
            end
      in
        if not (L.null recursiveWithoutReturnType) then
          raiseRecursiveFunctionsNeedExplicitReturnType ()
        else
          let
            val augmentedVenv = L.foldl augment venv fundecsWithRecursiveFlag
            val newVenv = L.foldl funDecMapper augmentedVenv fundecsWithRecursiveFlag
          in
            { venv = newVenv, tenv = tenv }
          end
      end
    | Ast.VarDec { name, escape, typ, init, pos } =>
      let
        fun tvarMapper (var, pos) =
          case Symbol.get tenv var of
            NONE => error pos ("type not found: "^ Symbol.name var)
          | SOME(t) => t
        val tvar = Option.map tvarMapper typ
        val { exp = (), ty = tinit } = translateExp venv tenv init { insideLoop = insideLoop }
        val varEntry =
          case tvar of
            NONE => Env.VarEntry { ty = tinit }
          | SOME(tvar) =>
            if Types.areEqual (tvar, tinit) then
              Env.VarEntry { ty = tinit }
            else
              error pos ("variable declaration type mismatch\n"
                       ^ "  required: "^ Syntax.showType tvar ^"\n"
                       ^ "     found: "^ Syntax.showType tinit ^"\n")
        val newVenv = Symbol.set venv name varEntry
      in
        { venv = newVenv, tenv = tenv }
      end

  and translateTy tenv ty pos =
    let
      fun aliasType (alias, pos) =
        case Symbol.get tenv alias of
          NONE => error pos ("type not found: "^ Symbol.name alias)
        | SOME(ty) => ty
      fun recordType fields =
        let
          fun mapper (Ast.Field { name, escape, typ, pos }) =
            case Symbol.get tenv typ of
              NONE => error pos ("type not found: "^ Symbol.name typ)
            | SOME(t) => (name, t)
          val recordFields = L.map mapper fields
        in
          Types.RECORD(recordFields, ref ())
        end
      fun arrayType innerType pos =
        case Symbol.get tenv innerType of
          NONE => error pos ("type not found: "^ Symbol.name innerType)
        | SOME(innerType) => Types.ARRAY(innerType, ref ())
    in
      case ty of
        Ast.NameTy(alias) => aliasType alias
      | Ast.RecordTy(fields) => recordType fields
      | Ast.ArrayTy(innerType, pos) => arrayType innerType pos
    end

  fun translateProgram ast =
    let
      val { exp, ty } = translateExp Env.base_venv Env.base_tenv ast { insideLoop = false }
    in
      print ("type: "^ (Syntax.showType ty) ^"\n")
    end
    handle
      TypeError(pos, message) =>
        print ("type error [line "^ (Int.toString pos) ^"]: "^ message ^"\n")
end

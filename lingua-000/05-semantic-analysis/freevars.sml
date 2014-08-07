structure FreeVars : FREE_VARS =
struct
  local
    structure L = List

    fun varFrees var venv free =
      case var of
        Ast.FieldVar(var, _, _) => varFrees var venv free
      | Ast.SubscriptVar(var, exp, _) =>
          expFrees exp venv (varFrees var venv free)
      | Ast.SimpleVar(symbol, _) =>
          if Symbol.has venv symbol then free else Symbol.set free symbol ()

    (*
     * This function returns not only the free variables encountered, but also
     * an augmented environment, which callers can use, for example, when
     * evaluating `LetExp` expressions.
     *)
    and decFrees dec venv free =
      case dec of
        Ast.TypeDec(_) => (venv, free)
      | Ast.VarDec { name, init, ... } =>
          let
            val free' = expFrees init venv free
            val venv' = Symbol.set venv name ()
          in
            (venv', free')
          end
      | Ast.FunctionDec(fundecs) =>
          let
            fun gatherFreeVars (Ast.FunDec { params, body, ... }, free) =
              let
                fun addParam (Ast.Field { name, ... }, venv) = Symbol.set venv name ()
                val venvWithParams = L.foldl addParam venv params
                val free' = expFrees body venvWithParams free
              in
                free'
              end
            val freeVars = L.foldl gatherFreeVars free fundecs
          in
            (venv, freeVars)
          end

    and expFrees exp venv free =
      let
        fun freeCallExp func args =
          let
            val freeArgs = L.foldl (fn (arg, free) => expFrees arg venv free) free args
          in
            if Symbol.has venv func then freeArgs
            else Symbol.set freeArgs func ()
          end

        fun freeOpExp left right =
          expFrees right venv (expFrees left venv free)

        fun freeRecordExp fields =
          let
            fun folder ((_, exp, _), free) = expFrees exp venv free
          in
            L.foldl folder free fields
          end

        fun freeSeqExp exps =
          let
            fun folder ((exp, _), free) = expFrees exp venv free
          in
            L.foldl folder free exps
          end

        fun freeAssignExp var exp =
          expFrees exp venv (varFrees var venv free)

        fun freeIfExp test thn els =
          let
            val thenFree = expFrees thn venv (expFrees test venv free)
          in
            Option.getOpt (Option.map (fn els => expFrees els venv thenFree) els, thenFree)
          end

        fun freeWhileExp test body =
          expFrees body venv (expFrees test venv free)

        fun freeForExp var lo hi body =
          let
            val freeLo = expFrees lo venv free
            val freeHi = expFrees hi venv freeLo
            val venv' = Symbol.set venv var ()
          in
            expFrees body venv' freeHi
          end

        fun freeLetExp decs body =
          let
            fun augment (dec, (venv, free)) = decFrees dec venv free
            val (venv', free') = L.foldl augment (venv, free) decs
          in
            expFrees body venv' free'
          end

        fun freeArrayExp size init =
          expFrees size venv (expFrees size venv free)
      in
        case exp of
          Ast.VarExp(var)                       => varFrees var venv free
        | Ast.NilExp                            => free
        | Ast.IntExp(_)                         => free
        | Ast.StringExp(_, _)                   => free
        | Ast.CallExp { func, args, ... }       => freeCallExp func args
        | Ast.OpExp { left, right, ... }        => freeOpExp left right
        | Ast.RecordExp { fields, ... }         => freeRecordExp fields
        | Ast.SeqExp(exps)                      => freeSeqExp exps
        | Ast.AssignExp { var, exp, ... }       => freeAssignExp var exp
        | Ast.IfExp { test, then', else', ... } => freeIfExp test then' else'
        | Ast.WhileExp { test, body, ... }      => freeWhileExp test body
        | Ast.ForExp { var, lo, hi, body, ... } => freeForExp var lo hi body
        | Ast.BreakExp(_)                       => free
        | Ast.LetExp { decs, body, ... }        => freeLetExp decs body
        | Ast.ArrayExp { typ, size, init, ... } => freeArrayExp size init
      end
  in
    fun freeVarsInVar var = varFrees var Symbol.empty Symbol.empty
    fun freeVarsInExp exp = expFrees exp Symbol.empty Symbol.empty
    fun freeVarsInDec dec = #2 (decFrees dec Symbol.empty Symbol.empty)
  end
end

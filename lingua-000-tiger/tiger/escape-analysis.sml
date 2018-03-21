structure EscapeAnalysis :> ESCAPE_ANALYSIS =
struct
  open Fn
  infix 1 |>

  fun analyse program =
    let
      type depth = int
      type env = (depth * bool ref) Symbol.table
      val initialEnv : env = Symbol.empty
      val initialDepth : depth = 0
    in
      analyseExp initialEnv initialDepth program
    end

  and analyseExp env depth exp =
    case exp of
      Ast.NilExp => ()
    | Ast.IntExp _ => ()
    | Ast.BreakExp _ => ()
    | Ast.StringExp _ => ()
    | Ast.VarExp var => analyseVar env depth var
    | Ast.CallExp { args, ... } => analyseExps env depth args
    | Ast.OpExp { left, right, ... } => analyseExps env depth [left, right]
    | Ast.RecordExp { fields, ... } => List.map #2 fields |> analyseExps env depth
    | Ast.SeqExp exps => List.map #1 exps |> analyseExps env depth
    | Ast.AssignExp { var, exp, ... } => (analyseVar env depth var ; analyseExp env depth exp)
    | Ast.WhileExp { test, body, ... } => analyseExps env depth [test, body]
    | Ast.ArrayExp { size, init, ... } => analyseExps env depth [size, init]
    | Ast.IfExp { test, then', else', ... } => List.app (Option.app (analyseExp env depth)) [SOME test, SOME then', else']
    | Ast.LetExp { decs, body, ... } => analyseExp (analyseDecs env depth decs) depth body
    | Ast.ForExp { var, escape, lo, hi, body, ... } =>
      let
        val bodyEnv = Symbol.set env var (depth, escape)
      in
        analyseExps env depth [lo, hi]
      ; analyseExp bodyEnv depth body
      end

  and analyseExps env depth exps = List.app (analyseExp env depth) exps

  and analyseVar env depth var =
    case var of
      Ast.FieldVar (var, _, _) => analyseVar env depth var
    | Ast.SubscriptVar (var, exp, _) => (analyseVar env depth var ; analyseExp env depth exp)
    | Ast.SimpleVar (name, _) =>
        case Symbol.get env name of
          NONE => () (* Let Semant report unbound variables. *)
        | SOME (declarationDepth, escape) => escape := declarationDepth < depth

  and analyseDec env depth decs =
    case decs of
      Ast.TypeDec types => env
    | Ast.VarDec { name, escape, init, ... } => (analyseExp env depth init ; Symbol.set env name (depth, escape))
    | Ast.FunctionDec fundecs =>
      let
        fun addParamToEnv (Ast.Field { name, escape, ... }, env) =
          Symbol.set env name (depth + 1, escape)
        fun folder (Ast.FunDec { params, body, ... }, env) =
          let
            val bodyEnv = List.foldl addParamToEnv env params
          in
            analyseExp bodyEnv (depth + 1) body
          ; env
          end
      in
        List.foldl folder env fundecs
      end

  and analyseDecs env depth decs =
    List.foldl (fn (dec, env) => analyseDec env depth dec) env decs
end

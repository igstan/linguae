structure Env : ENV =
struct
  type ty = Types.ty

  datatype enventry =
    VarEntry of {
      access : Translate.access,
      ty : ty
    }
  | FunEntry of {
      level : Translate.level,
      label : Temp.label,
      formals : ty list,
      result : ty
    }

  val quux = Types.RECORD([
    (Symbol.symbol "foo", Types.INT),
    (Symbol.symbol "bar", Types.STRING)
  ], ref ())

  val base_tenv =
    let
      val tenv0 = Symbol.empty
      val tenv1 = Symbol.set tenv0 (Symbol.symbol "nil") Types.NIL
      val tenv2 = Symbol.set tenv1 (Symbol.symbol "unit") Types.UNIT
      val tenv3 = Symbol.set tenv2 (Symbol.symbol "int") Types.INT
      val tenv4 = Symbol.set tenv3 (Symbol.symbol "string") Types.STRING
      val tenv5 = Symbol.set tenv4 (Symbol.symbol "quux") quux
    in
      tenv5
    end

  val base_venv =
    let
      val venv0 = Symbol.empty
      val venv1 = Symbol.set venv0 (Symbol.symbol "print") (FunEntry {
        level = Translate.topLevel,
        label = Temp.newLabel (),
        formals = [Types.STRING],
        result = Types.UNIT
      })
      val venv2 = Symbol.set venv1 (Symbol.symbol "q") (VarEntry {
        ty = quux,
        access = Translate.allocLocal Translate.topLevel true
      })
      val venv3 = Symbol.set venv2 (Symbol.symbol "arr") (VarEntry {
        ty = Types.ARRAY(Types.STRING, ref ()),
        access = Translate.allocLocal Translate.topLevel true
      })
    in
      venv3
    end
end

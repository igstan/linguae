signature TRANSLATE =
sig
  structure Frame : FRAME

  type level
  type access

  type frag = Frame.frag
  type offset = int

  datatype exp =
    Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of Temp.label * Temp.label -> Tree.stm

  val topLevel : level
  val newLevel : { parent : level, name : Temp.label, formals : bool list } -> level

  val formals : level -> access list
  val allocLocal : level -> bool -> access

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> Temp.label * Temp.label -> Tree.stm

  val nilExp : exp
  val simpleVar : access * level -> exp
  val fieldVar : exp * offset -> exp
  val subscriptVar : exp * exp -> exp
  val callExp : Temp.label * level * level * exp list -> exp
  val opExp : Ast.oper * exp * exp -> exp
  val recordExp : exp list -> exp
  val seqExp : exp list -> exp
  val assignExp : exp * exp -> exp
  val ifExp : (exp * exp * exp option) -> exp
  val whileExp : (exp * exp * Temp.label) -> exp
  val forExp : (exp * exp * exp * Temp.label) -> exp
  val letExp : (exp list * exp) -> exp
  val arrayExp : (exp * exp) -> exp
  val breakExp : Temp.label -> exp
  val intExp : int -> exp
  val stringExp : string -> exp
  val varDec : exp -> exp
  val funDec : (level * exp) -> exp

  val procEntryExit : { level : level, body : exp } -> unit
  val getResult : unit -> frag list
end

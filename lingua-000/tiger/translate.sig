signature TRANSLATE =
sig
  type level
  type access

  datatype exp =
    Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of Temp.label * Temp.label -> Tree.stm

  val outermost : level
  val newLevel : { parent : level, name : Temp.label, formals : bool list } -> level

  val formals : level -> access list
  val allocLocal : level -> bool -> access

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> Temp.label * Temp.label -> Tree.stm
end

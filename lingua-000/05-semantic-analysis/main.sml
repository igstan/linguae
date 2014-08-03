structure Main =
struct

  fun main () =
    let
      val a = Ast.OpExp { left=Ast.IntExp(1), oper=Ast.PlusOp, right=Ast.StringExp("2", 1), pos=1 }
      val t = Semant.translateProgram a
    in

    end

end

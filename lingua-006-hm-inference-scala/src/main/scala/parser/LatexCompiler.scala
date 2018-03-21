package codecamp
package parser

/**
 * The tree diagrams in the presentation have been generated using this.
 */
object LatexCompiler {
  def compile(args: Array[String]): Unit = {
    val term = Parser.parse(Scanner.scan("""
      let
        val const = fn y =>
          let
            val f = fn x => y
          in
            f
          end
      in
        (const true 1) + 2
      end
    """))

    // val latex = compileTermTree(term)
    Type.resetFreshness()
    val latex = compileTypedTermTree(Annotate.annotate(term, TypeEnv(Map(
      "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
      "-" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT))
    ))))

    println(latex)
  }

  def compileTermTree(term: Term): String = {
    s"\\Tree ${compileTerm(term)}"
  }

  def compileTypedTermTree(typedTerm: TypedTerm): String = {
    s"\\Tree ${compileTypedTerm(typedTerm)}"
  }

  def compileTerm(term: Term): String = {
    term match {
      case INT(value) =>
        s"[.INT ${value.toString} ]"
      case BOOL(value) =>
        s"[.BOOL ${value.toString} ]"
      case VAR(name) =>
        s"[.VAR $name ]"
      case IF(test, yes, no) =>
        val treeTest = compileTerm(test)
        val treeYes = compileTerm(yes)
        val treeNo = compileTerm(no)
        s"[.IF $treeTest $treeYes $treeNo ]"
      case FUN(param, body) =>
        val treeBody = compileTerm(body)
        s"[.FUN $param $treeBody ]"
      case APP(fn, arg) =>
        val treeFn = compileTerm(fn)
        val treeArg = compileTerm(arg)
        s"[.APP $treeFn $treeArg ]"
      case LET(binding, value, body) =>
        val treeValue = compileTerm(value)
        val treeBody = compileTerm(body)
        s"[.LET $binding $treeValue $treeBody ]"
    }
  }

  def compileTypedTerm(typedTerm: TypedTerm): String = {
    import TypedTerm._

    def subscript(ty: Type): String = {
      ty match {
        case Type.INT => "{int}"
        case Type.BOOL => "{bool}"
        case Type.FUN(paramTy, returnTy) => "{" + subscript(paramTy) + "} \\rightarrow " + subscript(returnTy)
        case Type.VAR(index) => "{t_{" + index.toString + "}}"
      }
    }

    def style(ty: Type, node: String): String = {
      s"$${\\texttt{\\color{blue}" + node + "}}_{\\color{red}"+ subscript(ty) +"}$"
    }

    def binder(b: Binder): String = {
      s"$${\\texttt{\\color{black}" + b.name + "}}_{\\color{red}"+ subscript(b.ty) +"}$"
    }

    typedTerm match {
      case INT(ty, value) =>
        s"""[.${style(ty, "INT")} ${value.toString} ]"""
      case BOOL(ty, value) =>
        s"""[.${style(ty, "BOOL")} ${value.toString} ]"""
      case VAR(ty, name) =>
        s"""[.${style(ty, "VAR")} $name ]"""
      case IF(ty, test, yes, no) =>
        val treeTest = compileTypedTerm(test)
        val treeYes = compileTypedTerm(yes)
        val treeNo = compileTypedTerm(no)
        s"""[.${style(ty, "IF")} $treeTest $treeYes $treeNo ]"""
      case FUN(ty, param, body) =>
        val treeBody = compileTypedTerm(body)
        s"""[.${style(ty, "FUN")} ${binder(param)} $treeBody ]"""
      case APP(ty, fn, arg) =>
        val treeFn = compileTypedTerm(fn)
        val treeArg = compileTypedTerm(arg)
        s"""[.${style(ty, "APP")} $treeFn $treeArg ]"""
      case LET(ty, binding, value, body) =>
        val treeValue = compileTypedTerm(value)
        val treeBody = compileTypedTerm(body)
        s"""[.${style(ty, "LET")} ${binder(binding)} $treeValue $treeBody ]"""
    }
  }
}

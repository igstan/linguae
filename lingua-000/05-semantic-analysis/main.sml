structure Main =
struct
  fun main () =
    let
      (*
       * The following AST represents this Tiger program:
       *
       * let
       *   type number = int
       *   type intArray = array of int
       *   var r : number := 42
       *   var someArray : intArray = intArray[10] of 0
       *   function double(a : number) : number = a + a
       * in
       *   (for i := 1 to 10 do i)
       * end
       *)
      val ast = Ast.LetExp {
        decs = [
          Ast.TypeDec([
            {
              name = Symbol.symbol "number",
              ty = Ast.NameTy(Symbol.symbol "int", 42),
              pos = 42
            },
            {
              name = Symbol.symbol "intArray",
              ty = Ast.ArrayTy(Symbol.symbol "int", 42),
              pos = 42
            }
          ]),
          Ast.VarDec {
            name = Symbol.symbol "r",
            escape = ref true,
            typ = SOME(Symbol.symbol "number", 42),
            init = Ast.IntExp(15),
            pos = 42
          },
          Ast.VarDec {
            name = Symbol.symbol "someArray",
            escape = ref true,
            typ = SOME((Symbol.symbol "intArray", 42)),
            init = Ast.ArrayExp {
              typ = Symbol.symbol "intArray",
              size = Ast.IntExp(10),
              init = Ast.IntExp(0),
              pos = 42
            },
            pos = 42
          },
          Ast.FunctionDec([
            Ast.FunDec {
              name = Symbol.symbol "double",
              params = [
                Ast.Field {
                  name = Symbol.symbol "a",
                  escape = ref true,
                  typ = Symbol.symbol "number",
                  pos = 42
                }
              ],
              result = SOME(Symbol.symbol "number", 42),
              body = Ast.OpExp {
                left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "a", 42)),
                oper = Ast.PlusOp,
                right = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "a", 42)),
                pos = 42
              },
              pos = 42
            }
          ])
        ],
        body = Ast.ForExp {
          var = Symbol.symbol "i",
          escape = ref true,
          lo = Ast.IntExp(1),
          hi = Ast.IntExp(10),
          body = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "i", 42)),
          pos = 42
        },
        pos = 42
      };
    in
      Semant.translateProgram ast
    end
end

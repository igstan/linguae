structure Test =
struct
  fun main () =
    let
      (*
       * The AST below represents this Tiger program:
       *
       * let
       *   type number = int
       *   type intArray = array of int
       *   type list = { head: int, tail: list }
       *   type peek = { peek : int, poke : poke }
       *   type poke = { poke : int, peek : peek }
       *   var r : number := 42
       *   var someArray : intArray = intArray[10] of 0
       *   function double(a : number) : number = a + a
       *   function factorial(n : int) : int =
       *     if n = 0 then 1 else n * fact(n - 1)
       *   function odd(n : int) : int = if n = 0 then 0 else even(n - 1)
       *   function even(n : int) : int = if n = 0 then 1 else odd(n - 1)
       * in
       *   (for i := 1 to 10 do i);
       *   { head = 1, tail = nil }
       * end
       *)
      val forExp = Ast.ForExp {
        var = Symbol.symbol "i",
        escape = ref true,
        lo = Ast.IntExp(1),
        hi = Ast.IntExp(10),
        (* body = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "i", 42)), *)
        body = Ast.BreakExp(42),
        pos = 42
      }
      val listRecord = Ast.RecordExp {
        fields = [
          (Symbol.symbol "head", Ast.IntExp(1), 42),
          (Symbol.symbol "tail", Ast.NilExp, 42)
        ],
        name = Symbol.symbol "list",
        pos = 42
      }
      val peekPoke = Ast.RecordExp {
        fields = [
          (Symbol.symbol "peek", Ast.IntExp(1), 42),
          (Symbol.symbol "poke", Ast.NilExp, 42)
        ],
        name = Symbol.symbol "peek",
        pos = 42
      }
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
            },
            {
              name = Symbol.symbol "list",
              ty = Ast.RecordTy([
                Ast.Field {
                  name = Symbol.symbol "head",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                },
                Ast.Field {
                  name = Symbol.symbol "tail",
                  escape = ref true,
                  typ = Symbol.symbol "list",
                  pos = 42
                }
              ]),
              pos = 42
            },
            {
              name = Symbol.symbol "peek",
              ty = Ast.RecordTy([
                Ast.Field {
                  name = Symbol.symbol "peek",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                },
                Ast.Field {
                  name = Symbol.symbol "poke",
                  escape = ref true,
                  typ = Symbol.symbol "poke",
                  pos = 42
                }
              ]),
              pos = 42
            },
            {
              name = Symbol.symbol "poke",
              ty = Ast.RecordTy([
                Ast.Field {
                  name = Symbol.symbol "poke",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                },
                Ast.Field {
                  name = Symbol.symbol "peek",
                  escape = ref true,
                  typ = Symbol.symbol "peek",
                  pos = 42
                }
              ]),
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
            },
            Ast.FunDec {
              name = Symbol.symbol "factorial",
              params = [
                Ast.Field {
                  name = Symbol.symbol "n",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                }
              ],
              result = SOME(Symbol.symbol "int", 42),
              body = Ast.IfExp {
                test = Ast.OpExp {
                  left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                  oper = Ast.EqOp,
                  right = Ast.IntExp(0),
                  pos = 42
                },
                then' = Ast.IntExp(1),
                else' = SOME(Ast.OpExp {
                  left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                  oper = Ast.TimesOp,
                  right = Ast.CallExp {
                    func = Symbol.symbol "factorial",
                    args = [
                      Ast.OpExp {
                        left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                        oper = Ast.MinusOp,
                        right = Ast.IntExp(1),
                        pos = 42
                      }
                    ],
                    pos = 42
                  },
                  pos = 42
                }),
                pos = 42
              },
              pos = 42
            },
            Ast.FunDec {
              name = Symbol.symbol "odd",
              params = [
                Ast.Field {
                  name = Symbol.symbol "n",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                }
              ],
              result = SOME(Symbol.symbol "int", 42),
              body = Ast.IfExp {
                test = Ast.OpExp {
                  left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                  oper = Ast.EqOp,
                  right = Ast.IntExp(0),
                  pos = 42
                },
                then' = Ast.IntExp(1),
                else' = SOME(Ast.CallExp {
                  func = Symbol.symbol "even",
                  args = [
                    Ast.OpExp {
                      left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                      oper = Ast.MinusOp,
                      right = Ast.IntExp(1),
                      pos = 42
                    }
                  ],
                  pos = 42
                }),
                pos = 42
              },
              pos = 42
            },
            Ast.FunDec {
              name = Symbol.symbol "even",
              params = [
                Ast.Field {
                  name = Symbol.symbol "n",
                  escape = ref true,
                  typ = Symbol.symbol "int",
                  pos = 42
                }
              ],
              result = SOME(Symbol.symbol "int", 42),
              body = Ast.IfExp {
                test = Ast.OpExp {
                  left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                  oper = Ast.EqOp,
                  right = Ast.IntExp(0),
                  pos = 42
                },
                then' = Ast.IntExp(0),
                else' = SOME(Ast.CallExp {
                  func = Symbol.symbol "odd",
                  args = [
                    Ast.OpExp {
                      left = Ast.VarExp(Ast.SimpleVar(Symbol.symbol "n", 42)),
                      oper = Ast.MinusOp,
                      right = Ast.IntExp(1),
                      pos = 42
                    }
                  ],
                  pos = 42
                }),
                pos = 42
              },
              pos = 42
            }
          ])
        ],
        body = Ast.SeqExp([
          (forExp, 42),
          (listRecord, 42),
          (peekPoke, 42)
        ]),
        pos = 42
      }

      (*
       * let
       *   var foo : int := 13
       *   function bar() : int = 1 + foo
       * in
       *   nil
       * end
       *)
      val escapingVar = Ast.LetExp {
        decs = [
          Ast.VarDec {
            name = Symbol.symbol "foo",
            escape = ref false,
            typ = SOME (Symbol.symbol "int", 42),
            init = Ast.IntExp 13,
            pos = 42
          },
          Ast.FunctionDec [
            Ast.FunDec {
              name = Symbol.symbol "bar",
              params = [],
              result = SOME (Symbol.symbol "int", 42),
              body = Ast.OpExp {
                left = Ast.IntExp 1,
                oper = Ast.PlusOp,
                right = Ast.VarExp (Ast.SimpleVar (Symbol.symbol "foo", 42)),
                pos = 42
              },
              pos = 42
            }
          ]
        ],
        body = Ast.NilExp,
        pos = 42
      }
    in
      (* Semant.translateProgram ast *)
      print (ShowAst.showExp escapingVar)
    ; print "\n——————————————————————————————————————————————————————————————\n"
    ; EscapeAnalysis.analyse escapingVar
    ; print (ShowAst.showExp escapingVar)
    end
end

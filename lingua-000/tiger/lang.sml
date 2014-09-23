infixr 1 <|
infixr 9 <|>

structure Lang =
struct
  (* Low-precedence function application. Useful to avoid parentheses. *)
  fun f <| x = f x

  (* Function composition. The built-in `o` operator is pretty hard to spot. *)
  fun f <|> g = fn (x) => f (g x)

  (* Constant function. *)
  fun const a b = a

  (* Identity function. *)
  fun id a = a
end

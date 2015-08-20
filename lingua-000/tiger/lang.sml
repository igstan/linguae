infix 1 |>

structure Lang =
struct
  (* Reverse function application. Useful to pipe results from call to call. *)
  fun x |> f = f x

  (* Constant function. *)
  fun const a b = a

  (* Identity function. *)
  fun id a = a
end

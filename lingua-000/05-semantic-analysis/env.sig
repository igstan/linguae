signature ENV =
sig

  type ty

  datatype enventry =
    VarEntry of { ty: ty }
  | FunEntry of { formals: ty list, result: ty }

  (* Predefined types. *)
  val base_tenv : ty Symbol.table

  (* Predefined functions. *)
  val base_venv : enventry Symbol.table

end

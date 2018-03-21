signature ENV =
sig
  type ty

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

  (* Predefined types. *)
  val base_tenv : ty Symbol.table

  (* Predefined values. *)
  val base_venv : enventry Symbol.table
end

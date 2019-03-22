signature ENV =
sig
    type level
    type access
    type ty
    datatype enventry = VarEntry of {access: access, ty: ty, forIdx: bool}
                      | FunEntry of {level: level,
                                     label: Temp.label,
                                     formals: ty list,
                                     result : ty}
    val base_tenv: ty Symbol.table          (* predefined types *)
    val base_venv: enventry Symbol.table    (* predefined functions *)
end

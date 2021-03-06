signature ENV =
sig
    structure Translate : TRANSLATE
    type ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty, forIdx: bool}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list,
                                     result : ty}
    val base_tenv: ty Symbol.table          (* predefined types *)
    val base_venv: enventry Symbol.table    (* predefined functions *)
end

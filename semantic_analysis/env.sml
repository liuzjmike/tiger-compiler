structure Env: ENV =
struct
    type access = int
    type ty = Types.ty
    datatype enventry = VarEntry of {ty: ty, forIdx: bool}
                      | FunEntry of {formals: ty list, result: ty}
    val base_tenv = Symbol.empty
    val base_venv = Symbol.empty
end
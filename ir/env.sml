structure Env: ENV =
struct

    structure S = Symbol
    structure T = Types

    type access = int
    type ty = T.ty
    datatype enventry = VarEntry of {ty: ty, forIdx: bool}
                      | FunEntry of {formals: ty list, result: ty}

    fun addToTable ((k, v), t) = S.enter (t, S.symbol k, v)

    val base_tenv =
        foldl addToTable S.empty
        [("int", T.INT), ("string", T.STRING)]
    val base_venv =
        foldl addToTable S.empty [
            ("print", FunEntry {formals=[T.STRING], result=T.UNIT}),
            ("flush", FunEntry {formals=[], result=T.UNIT}),
            ("getchar", FunEntry {formals=[], result=T.STRING}),
            ("ord", FunEntry {formals=[T.STRING], result=T.INT}),
            ("chr", FunEntry {formals=[T.INT], result=T.STRING}),
            ("size", FunEntry {formals=[T.STRING], result=T.INT}),
            ("substring", FunEntry {formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
            ("concat", FunEntry {formals=[T.STRING, T.STRING], result=T.STRING}),
            ("not", FunEntry {formals=[T.INT], result=T.INT}),
            ("exit", FunEntry {formals=[T.INT], result=T.UNIT})
        ]

end

functor Env (Translate : TRANSLATE) : ENV =
struct

    structure S = Symbol
    structure T = Types

    structure Translate = Translate

    type level = Translate.level
    type access = Translate.access
    type ty = T.ty
    datatype enventry = VarEntry of {access: access, ty: ty, forIdx: bool}
                      | FunEntry of {level: level,
                                     label: Temp.label,
                                     formals: ty list,
                                     result : ty}

    fun addToTable ((k, v), t) = S.enter (t, S.symbol k, v)

    val base_tenv =
        foldl addToTable S.empty
        [("int", T.INT), ("string", T.STRING)]
    val base_venv =
        foldl addToTable S.empty [
            ("print", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "print",
                formals=[T.STRING],
                result=T.UNIT
            }),
            ("flush", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "flush",
                formals=[],
                result=T.UNIT
            }),
            ("getchar", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "getchar",
                formals=[],
                result=T.STRING
            }),
            ("ord", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "ord",
                formals=[T.STRING],
                result=T.INT
            }),
            ("chr", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "chr",
                formals=[T.INT],
                result=T.STRING
            }),
            ("size", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "size",
                formals=[T.STRING],
                result=T.INT
            }),
            ("substring", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "substring",
                formals=[T.STRING, T.INT, T.INT],
                result=T.STRING
            }),
            ("concat", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "concat",
                formals=[T.STRING, T.STRING],
                result=T.STRING
            }),
            ("not", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "not",
                formals=[T.INT],
                result=T.INT
            }),
            ("exit", FunEntry {
                level=Translate.outermost,
                label=Temp.namedlabel "exit",
                formals=[T.INT],
                result=T.UNIT
            })
        ]

end

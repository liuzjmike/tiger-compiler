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
                label=Temp.newlabel (),
                formals=[T.STRING],
                result=T.UNIT
            }),
            ("flush", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[],
                result=T.UNIT
            }),
            ("getchar", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[],
                result=T.STRING
            }),
            ("ord", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.STRING],
                result=T.INT
            }),
            ("chr", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.INT],
                result=T.STRING
            }),
            ("size", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.STRING],
                result=T.INT
            }),
            ("substring", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.STRING, T.INT, T.INT],
                result=T.STRING
            }),
            ("concat", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.STRING, T.STRING],
                result=T.STRING
            }),
            ("not", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.INT],
                result=T.INT
            }),
            ("exit", FunEntry {
                level=Translate.outermost,
                label=Temp.newlabel (),
                formals=[T.INT],
                result=T.UNIT
            })
        ]

end

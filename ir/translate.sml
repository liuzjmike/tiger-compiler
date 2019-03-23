functor Translate (F : FRAME) : TRANSLATE =
struct
    structure T = Tree

    datatype exp = Ex of T.exp
                 | Nx of T.stm
                 | Cx of Temp.label * Temp.label -> T.stm
    datatype level = Level of {parent: level, frame: F.frame, id: unit ref}
                   | Outermost
    type access = level * F.access

    val outermost = Outermost

    fun newLevel {parent, name, formals} = Level {
        parent=parent,
        frame=F.newFrame {name=name, formals=true::formals},
        id=ref ()
    }

    fun formals level = (
        case level
        of  Level {parent, frame, id} => map (fn a => (level, a)) (F.formals frame)
        |   Outermost => ErrorMsg.impossible "access formals of the outermost level"
    )

    fun allocLocal level escape = (
        case level
        of  Level {parent, frame, id} => (level, F.allocLocal frame escape)
        |   Outermost => ErrorMsg.impossible "allocate local variable in the outermost level"
    )

    fun simpleVar ((defLevel, access), curLevel) =
        let val defId = case defLevel
            of  Level {parent, frame, id} => id
            |   Outermost => ErrorMsg.impossible "variable defined in the outermost level"
            fun unLevel (Level level) = level
            |   unLevel Outermost = ErrorMsg.impossible "access the outermost level"
            fun g ({parent, frame, id}, frameAddr) =
                if id = defId
                then F.exp access (frameAddr)
                else g (unLevel parent, F.exp (List.hd (F.formals frame)) frameAddr)
        in Ex (g (unLevel curLevel, T.TEMP F.FP))
        end

    fun unEx (Ex e) = e
    |   unEx (Nx s) = T.ESEQ (s, T.CONST 0)
    |   unEx (Cx genstm) =
        let val r = Temp.newtemp ()
            val t = Temp.newlabel ()
            val f = Temp.newlabel ()
        in 
            T.ESEQ (T.seq [
                    T.MOVE (T.TEMP r, T.CONST 1) ,
                    genstm (t, f),
                    T.LABEL f,
                    T.MOVE (T.TEMP r, T.CONST 0 ),
                    T. LABEL t
                ] ,
                T.TEMP r)
        end

    fun unNx (Nx s) = s
    |   unNx (Ex e) = T.EXP e
    |   unNx (Cx f) =
        let val l = Temp.newlabel ()
        in T.SEQ (f(l, l), T.LABEL l)
        end

    fun unCx (Cx f) = f
    |   unCx (Ex (T.CONST 1)) = (fn (t, f) => T.JUMP (T.NAME t, [t]))
    |   unCx (Ex (T.CONST 0)) = (fn (t, f) => T.JUMP (T.NAME f, [f]))
    |   unCx (Ex e) = (fn (t, f) => T.CJUMP (T.NE, e, T.CONST 0, t, f))
    |   unCx (Nx s) = ErrorMsg.impossible "convert statement into conditional"

end
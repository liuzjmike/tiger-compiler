functor Translate (Frame : FRAME) : TRANSLATE =
struct
    structure T = Tree

    datatype exp = Ex of T.exp
                 | Nx of T.stm
                 | Cx of Temp.label * Temp.label -> T.stm
    datatype level = Level of {parent: level, frame: Frame.frame}
                   | Outermost
    type access = level * Frame.access

    val outermost = Outermost

    fun newLevel {parent, name, formals} = Level {
        parent=parent,
        frame=Frame.newFrame {name=name, formals=true::formals}
    }

    fun formals level = (
        case level
        of  Level {parent, frame} => map (fn a => (level, a)) (Frame.formals frame)
        |   Outermost => ErrorMsg.impossible "access formals of the outermost level"
    )

    fun allocLocal level escape = (
        case level
        of  Level {parent, frame} => (level, Frame.allocLocal frame escape)
        |   Outermost => ErrorMsg.impossible "allocate local variable in the outermost level"
    )

    fun seq [] = ErrorMsg.impossible "build statement from empty statement list"
    |   seq [stm] = stm
    |   seq (stm::l) = T.SEQ (stm, seq l)

    fun unEx (Ex e) = e
    |   unEx (Nx s) = T.ESEQ (s, T.CONST 0)
    |   unEx (Cx genstm) =
        let val r = Temp.newtemp ()
            val t = Temp.newlabel ()
            val f = Temp.newlabel ()
        in 
            T.ESEQ (seq [
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
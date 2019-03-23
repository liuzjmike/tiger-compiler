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
                T.LVALUE (T.TEMP r))
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

    fun simpleVar ((defLevel, access), curLevel) =
        let val defId = case defLevel
            of  Level {parent, frame, id} => id
            |   Outermost => ErrorMsg.impossible "variable defined in the outermost level"
            fun unLevel (Level level) = level
            |   unLevel Outermost = ErrorMsg.impossible "access the outermost level"
            fun g ({parent, frame, id}, frameAddr) =
                if id = defId
                then F.exp access (T.LVALUE frameAddr)
                else g (unLevel parent, F.exp (List.hd (F.formals frame)) (T.LVALUE frameAddr))
        in Ex (T.LVALUE (g (unLevel curLevel, T.TEMP F.FP)))
        end

    (* fun fieldVar () *)

    fun nilExp () = T.MEM (T.CONST 0)

    fun intExp i = T.CONST i

    fun plusExp (left, right) = Ex (T.BINOP (T.PLUS, left, right))

    fun minusExp (left, right) = Ex (T.BINOP (T.MINUS, left, right))

    fun mulExp (left, right) = Ex (T.BINOP (T.MUL, left, right))

    fun divExp (left, right) = Ex (T.BINOP (T.DIV, left, right))

    fun eqExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.EQ, left, right, t, f))

    fun neExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.NE, left, right, t, f))

    fun ltExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.LT, left, right, t, f))

    fun leExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.LE, left, right, t, f))

    fun gtExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.GT, left, right, t, f))

    fun geExp (left, right) = Cx (fn (t, f) => T.CJUMP (T.GE, left, right, t, f))

    fun ifThenExp (test, then') =
        let val c = unCx test
            val t = unNx then'
            val trueLabel = Temp.newlabel ()
            val endLabel = Temp.newlabel ()
        in
            Nx (T.seq [
                c (trueLabel, endLabel),
                T.LABEL trueLabel,
                t,
                T.LABEL endLabel
            ])
        end

    fun ifThenElseExp (test, then', else') =
        let val c = unCx test
            val t = unEx then'
            val f = unEx else'
            val trueLabel = Temp.newlabel ()
            val falseLabel = Temp.newlabel ()
            val endLabel = Temp.newlabel ()
            val ans = Temp.newtemp ()
        in
            Ex (T.ESEQ (
                T.seq [
                    c (trueLabel, falseLabel),
                    T.LABEL trueLabel,
                    T.MOVE (T.TEMP ans, t),
                    T.JUMP (T.NAME endLabel, [endLabel]),
                    T.LABEL falseLabel,
                    T.MOVE (T.TEMP ans, f),
                    T.LABEL endLabel
                ],
                T.LVALUE (T.TEMP ans)))
        end

end
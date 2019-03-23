functor Translate (F : FRAME) : TRANSLATE =
struct
    structure T = Tree

    datatype exp = Ex of T.exp
                 | Nx of T.stm
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

    fun unNx (Nx s) = s
    |   unNx (Ex e) = T.EXP e

    (* During translation into assembly, if `e` is T.RELOP it should be munched directly;
    otherwise it should be compared to 0 *)
    fun branch (Ex (T.CONST 0)) = (fn (t, f) => T.JUMP (T.NAME f, [f]))
    |   branch (Ex (T.CONST c)) = (fn (t, f) => T.JUMP (T.NAME t, [t]))
    |   branch (Ex e) = (fn (t, f) => T.CJUMP (e, t, f))
    |   branch (Nx s) = ErrorMsg.impossible "convert statement into conditional branch"

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

    fun subscriptVar (a, i) = Ex (T.LVALUE (T.MEM (T.BINOP (T.PLUS, a, T.BINOP (T.MUL, i, T.CONST 4)))))

    fun nilExp () = T.MEM (T.CONST 0)

    fun intExp i = T.CONST i

    fun plusExp (left, right) = Ex (T.BINOP (T.PLUS, left, right))

    fun minusExp (left, right) = Ex (T.BINOP (T.MINUS, left, right))

    fun mulExp (left, right) = Ex (T.BINOP (T.MUL, left, right))

    fun divExp (left, right) = Ex (T.BINOP (T.DIV, left, right))

    (* TODO: Special treatment for comparing strings *)
    fun eqExp (left, right) = Ex (T.RELOP (T.EQ, left, right))

    fun neExp (left, right) = Ex (T.RELOP (T.NE, left, right))

    fun ltExp (left, right) = Ex (T.RELOP (T.LT, left, right))

    fun leExp (left, right) = Ex (T.RELOP (T.LE, left, right))

    fun gtExp (left, right) = Ex (T.RELOP (T.GT, left, right))

    fun geExp (left, right) = Ex (T.RELOP (T.GE, left, right))

    fun seqExp expList =
        let fun f [] = T.CONST 0
            |   f [exp] = unEx exp
            |   f (exp::l) = T.ESEQ (unNx exp, seqExp l)
        in f expList
        end

    fun ifThenExp (test, then') =
        let val c = branch test
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
        let val c = branch test
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

    fun whileExp (test, body) =
        let val b = unNx body
            val start = Temp.newlabel ()
            val end' = Temp.newlabel ()
            val testBranch = branch test (start, end')
        in
            (
                Nx (T.seq [
                    testBranch,
                    T.LABEL start,
                    b,
                    testBranch,
                    T.LABEL end'
                ]),
                end'
            )
        end

    fun forExp ((level, access), lo, hi, body) =
        let val i = F.exp access (T.LVALUE (T.TEMP F.FP))
            val hiReg = T.TEMP (Temp.newtemp ())
            val l = unEx lo
            val h = unEx hi
            val b = unNx body
            val beforeLabel = Temp.newlabel ()
            val bodyLabel = Temp.newlabel ()
            val afterLabel = Temp.newlabel ()
        in
            Nx (T.seq [
                T.MOVE (i, l),
                T.MOVE (hiReg, h),
                T.CJUMP (T.RELOP (T.LE, T.LVALUE i, T.LVALUE hiReg), bodyLabel, afterLabel),
                T.LABEL beforeLabel,
                T.MOVE (i, T.BINOP (T.PLUS, T.LVALUE i, T.CONST 1)),
                T.LABEL bodyLabel,
                b,
                T.CJUMP (T.RELOP (T.LT, T.LVALUE i, T.LVALUE hiReg), bodyLabel, afterLabel),
                T.LABEL afterLabel])
        end

end
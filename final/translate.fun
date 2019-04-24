functor Translate (F : FRAME) : TRANSLATE =
struct
    structure T = Tree

    datatype exp = Ex of T.exp
                 | Nx of T.stm
    datatype level = Level of {parent: level, frame: F.frame, id: unit ref}
                   | Outermost
    type access = level * F.access
    type frag = F.frag

    val outermost = Outermost

    val nilPointer = Temp.newlabel ()
    val indexOutOfBound = Temp.newlabel ()
    val defaultFragList = [
        F.STRING (nilPointer, "nil pointer\n"),
        F.STRING (indexOutOfBound, "array index out of bound\n")
    ]
    val fragList: F.frag list ref = ref defaultFragList

    fun newLevel {parent, name, formals} = Level {
        parent=parent,
        frame=F.newFrame {name=name, formals=true::formals},
        id=ref ()
    }

    fun formals level = (
        case level
        of  Level {parent, frame, id} => map (fn a => (level, a)) (List.tl (F.formals frame))
        |   Outermost => ErrorMsg.impossible "access formals of the outermost level"
    )

    fun allocLocal level escape = (
        case level
        of  Level {parent, frame, id} => (level, F.allocLocal frame escape)
        |   Outermost => ErrorMsg.impossible "allocate local variable in the outermost level"
    )

    fun unEx (Ex e) = e
    |   unEx (Nx (T.EXP e)) = e
    |   unEx (Nx s) = T.ESEQ (s, T.CONST 0)

    fun unNx (Nx s) = s
    |   unNx (Ex (T.ESEQ (s, T.CONST c))) = s
    |   unNx (Ex (T.ESEQ (s, T.TEMP t))) = s
    |   unNx (Ex e) = T.EXP e

    (* During translation into assembly, if `e` is T.RELOP it should be munched directly;
    otherwise it should be compared to 0 *)
    fun branch (Ex (T.CONST 0)) = (fn (t, f) => T.JUMP (T.NAME f, [f]))
    |   branch (Ex (T.CONST c)) = (fn (t, f) => T.JUMP (T.NAME t, [t]))
    |   branch (Ex e) = (fn (t, f) => T.CJUMP (e, t, f))
    |   branch (Nx s) = ErrorMsg.impossible "convert statement into conditional branch"

    fun unLevel (Level level) = level
    |   unLevel Outermost = ErrorMsg.impossible "access the outermost level"

    fun procEntryExit {level, body} =
        let val frame = #frame (unLevel level)
            val body = F.procEntryExit1 (frame, T.MOVE (T.TEMP F.RV, unEx body))
            val frag = F.PROC {body=body, frame=frame}
        in fragList := frag::(!fragList)
        end

    fun getResult () =
        let val result = !fragList
        in
            fragList := defaultFragList;
            result
        end

    fun staticLink frame frameAddr = F.exp (List.hd (F.formals frame)) frameAddr

    fun nilExp () = Ex (T.CONST 0)

    fun intExp i = Ex (T.CONST i)

    fun stringExp string =
        let val label = Temp.newlabel ()
        in
            fragList := (F.STRING (label, string))::(!fragList);
            Ex (T.NAME label)
        end

    fun callExp (args, caller, callee, label) =
        case callee
        of  Outermost => Ex (T.CALL (T.NAME label, map unEx args))
        |   Level {parent, frame, id} =>
            let fun sameLevel ({parent=p1, frame=f1, id=i1}, {parent=p2, frame=f2, id=i2}) = i1 = i2
                val calleeParent = #id (unLevel (parent))
                fun f ({parent, frame, id}, frameAddr) =
                    if id = calleeParent
                    then frameAddr
                    else f (unLevel parent, staticLink frame frameAddr)
                val sl = f (unLevel caller, T.TEMP F.FP)
            in
                Ex (T.CALL (T.NAME label, sl::(map unEx args)))
            end

    fun plusExp (left, right) = Ex (T.BINOP (T.PLUS, unEx left, unEx right))

    fun minusExp (left, right) = Ex (T.BINOP (T.MINUS, unEx left, unEx right))

    fun mulExp (left, right) = Ex (T.BINOP (T.MUL, unEx left, unEx right))

    fun divExp (left, right) = Ex (T.BINOP (T.DIV, unEx left, unEx right))

    fun eqExp (left, right, string) = Ex (
        if string
        then F.externalCall ("tig_stringEqual", [unEx left, unEx right])
        else T.RELOP (T.EQ, unEx left, unEx right)
    )

    fun neExp (left, right, string) = Ex (
        if string
        then T.BINOP (T.MINUS, T.CONST 1, F.externalCall ("tig_stringEqual", [unEx left, unEx right]))
        else T.RELOP (T.NE, unEx left, unEx right)
    )

    fun ltExp (left, right, string) = Ex (
        if string
        (* FIXME: add runtime function `stringLessThan` *)
        then F.externalCall ("stringLessThan", [unEx left, unEx right])
        else T.RELOP (T.LT, unEx left, unEx right)
    )

    fun leExp (left, right, string) = Ex (
        if string
        then T.BINOP (T.MINUS, T.CONST 1, F.externalCall ("stringLessThan", [unEx right, unEx left]))
        else T.RELOP (T.LE, unEx left, unEx right)
    )

    fun gtExp (left, right, string) = Ex (
        if string
        then F.externalCall ("stringLessThan", [unEx right, unEx left])
        else T.RELOP (T.GT, unEx left, unEx right)
    )

    fun geExp (left, right, string) = Ex (
        if string
        then T.BINOP (T.MINUS, T.CONST 1, F.externalCall ("stringLessThan", [unEx left, unEx right]))
        else T.RELOP (T.GE, unEx left, unEx right)
    )

    fun recordExp fields =
        let val a = T.TEMP (Temp.newtemp ())
            val extCall = F.externalCall ("tig_allocRecord", [T.CONST ((List.length fields) * F.wordSize)])
            fun f ([], k) = []
            |   f (field::l, k) =
                let val moveVal = T.MOVE (T.mem (a, T.CONST k), unEx field)
                in moveVal::(f (l, k+F.wordSize))
                end
        in
            Ex (T.ESEQ (T.seq (T.MOVE (a, extCall) :: (f (fields, 0))), a))
        end

    fun seqExp expList =
        let fun f [] = T.CONST 0
            |   f [exp] = unEx exp
            |   f (exp::l) = T.ESEQ (unNx exp, f l)
        in Ex (f expList)
        end

    fun assignExp (lvalue, value) = Nx (T.MOVE (unEx lvalue, unEx value))

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
                T.TEMP ans))
        end

    fun whileExp (test, body) =
        let val beforeLabel = Temp.newlabel ()
            val afterLabel = Temp.newlabel ()
            val b = unNx (body afterLabel)
            val testBranch = branch test (beforeLabel, afterLabel)
        in
            Nx (T.seq [
                testBranch,
                T.LABEL beforeLabel,
                b,
                testBranch,
                T.LABEL afterLabel
            ])
        end

    fun forExp ((level, access), lo, hi, body) =
        let val beforeLabel = Temp.newlabel ()
            val bodyLabel = Temp.newlabel ()
            val afterLabel = Temp.newlabel ()
            val l = unEx lo
            val h = unEx hi
            val b = unNx (body afterLabel)
            val i = F.exp access (T.TEMP F.FP)
            val hiReg = T.TEMP (Temp.newtemp ())
        in
            Nx (T.seq [
                T.MOVE (i, l),
                T.MOVE (hiReg, h),
                T.CJUMP (T.RELOP (T.LE, i, hiReg), bodyLabel, afterLabel),
                T.LABEL beforeLabel,
                T.MOVE (i, T.BINOP (T.PLUS, i, T.CONST 1)),
                T.LABEL bodyLabel,
                b,
                T.CJUMP (T.RELOP (T.LT, i, hiReg), beforeLabel, afterLabel),
                T.LABEL afterLabel])
        end

    fun breakExp label = Nx (T.JUMP (T.NAME label, [label]))

    fun letExp ([], body) = body
    |   letExp (varDecs, body) = Ex (T.ESEQ (T.seq (map unNx varDecs), unEx body))

    fun arrayExp (size, init) = Ex (F.externalCall ("tig_initArray", [unEx size, unEx init]))

    fun simpleVar ((defLevel, access), curLevel) =
        let val defId = case defLevel
            of  Level {parent, frame, id} => id
            |   Outermost => ErrorMsg.impossible "variable defined in the outermost level"
            fun g ({parent, frame, id}, frameAddr) =
                if id = defId
                then F.exp access frameAddr
                else g (unLevel parent, staticLink frame frameAddr)
        in Ex (g (unLevel curLevel, T.TEMP F.FP))
        end

    fun error strLabel =
        Nx (T.seq [
            T.EXP (F.externalCall ("tig_print", [T.NAME strLabel])),
            T.EXP (F.externalCall ("tig_flush", [])),
            T.EXP (F.externalCall ("tig_exit", [T.CONST 1]))
        ])

    fun fieldVar (a, i) = 
        let val addr = T.TEMP (Temp.newtemp ())
        in
            Ex (T.ESEQ ( T.SEQ (
                T.MOVE (addr, unEx a),
                unNx (ifThenExp (
                    Ex (T.RELOP (T.EQ, addr, T.CONST 0)),
                    (* NOTE: can implement runtime function `nilPointer` in runtime.c instead *)
                    error nilPointer
                ))),
                T.mem (addr, T.CONST (i * F.wordSize))
            ))
        end

    fun subscriptVar (a, i) = 
        let val addr = T.TEMP (Temp.newtemp ())
            val bound = T.MEM addr
            val idx = unEx i
            val offset = T.BINOP (
                T.MUL,
                T.BINOP (T.PLUS, idx, T.CONST 1),
                T.CONST F.wordSize
            )
        in
            Ex (T.ESEQ (T.SEQ (
                T.MOVE (addr, unEx a),
                unNx (ifThenExp (
                    Ex (T.BINOP (
                        T.OR,
                        T.RELOP (T.LT, idx, T.CONST 0),
                        T.RELOP (T.GE, idx, bound)
                    )),
                    (* NOTE: can implement runtime function `indexOutOfBound` in runtime.c instead *)
                    error indexOutOfBound
                ))),
                T.mem (addr, offset)
            ))
        end

    fun varDec ((level, access), value) = Nx (T.MOVE (F.exp access (T.TEMP F.FP), unEx value))

end
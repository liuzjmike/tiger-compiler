structure MipsGen : CODEGEN =
struct
    structure A = Assem
    structure T = Tree

    structure Frame = MipsFrame

    fun codegen frame stm =
        let val ilist = ref []
            fun emit x = ilist := x :: !ilist
            fun result gen =
                let val t = Temp.newtemp ()
                in gen t; t
                end
            
            fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2)), e)) =
                munchStm (T.MOVE (T.MEM (T.CONST (c1 + c2)), e))
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2)) =
                emit (A.OPER {
                    assem="sw `s0, " ^ Int.toString c ^ "(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c, e1)), e2)) =
                emit (A.OPER {
                    assem="sw `s0, " ^ Int.toString c ^ "(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.MEM (T.CONST c), e)) =
                emit (A.OPER {
                    assem="sw `s0, " ^ Int.toString c ^ "($zero)\n",
                    src=[munchExp e], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.MEM e1, e2)) =
                emit (A.OPER {
                    assem="assem=sw `s0, 0(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })

            and munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2))) =
                munchExp (T.CONST (c1 + c2))
            |   munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST c))) =
                result (fn r => emit (A.OPER {
                    assem="lw `d0, " ^ Int.toString c ^ "(`s0)\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.TEMP t) = t
            |   munchExp (T.CONST c) =
                result (fn r => emit (A.OPER {
                    assem="addi `d0, $zero, " ^ Int.toString c ^ "\n",
                    src=[], dst=[r], jump=NONE
                }))
        in rev (!ilist)
        end
end

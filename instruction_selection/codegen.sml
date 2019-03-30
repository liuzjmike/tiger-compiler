structure MipsGen : CODEGEN =
struct
    structure A = Assem
    structure T = Tree

    structure Frame = MipsFrame

    val zero = valOf (Frame.zero)

    fun codegen frame stm =
        let val ilist = ref []
            fun emit x = ilist := x :: !ilist
            fun result gen =
                let val t = Temp.newtemp ()
                in gen t; t
                end
            
            fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)
            (* MOVE *)
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2)), e)) =
                munchStm (T.MOVE (T.MEM (T.CONST (c1 + c2)), e))
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2)) =
                emit (A.OPER {
                    assem="sw `s0, " ^ Int.toString c ^ "(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c, e1)), e2)) =
                munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2))
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.MINUS, e1, T.CONST c)), e2)) =
                munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~c))), e2))
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
            |   munchStm (T.MOVE (T.TEMP t, e)) =
                emit (A.OPER {
                    assem="assem=add `d0, `s0, $zero\n",
                    src=[munchExp e], dst=[t], jump=NONE
                })
            |   munchStm (T.MOVE _) =
                ErrorMsg.impossible "illegal move statement"
            (* LABEL *)
            |   munchStm (T.LABEL label) =
                emit (A.LABEL {assem=Symbol.name label ^ ":\n", lab=label})

            and munchExp (T.ESEQ (s, e)) = (munchStm s; munchExp e)
            (* MEM *)
            |   munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2))) =
                munchExp (T.MEM (T.CONST (c1 + c2)))
            |   munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST c))) =
                result (fn r => emit (A.OPER {
                    assem="lw `d0, " ^ Int.toString c ^ "(`s0)\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST c, e))) =
                munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST c)))
            |   munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST c))) =
                munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST (~c))))
            |   munchExp (T.MEM (T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="lw `d0, " ^ Int.toString c ^ "($zero)\n",
                    src=[], dst=[r], jump=NONE
                }))
            |   munchExp (T.MEM e) =
                result (fn r => emit (A.OPER {
                    assem="lw `d0, 0(`s0)\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            (* PLUS *)
            |   munchExp (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 + c2))
            |   munchExp (T.BINOP (T.PLUS, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="addi `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.PLUS, T.CONST c, e)) =
                munchExp (T.BINOP (T.PLUS, e, T.CONST c))
            |   munchExp (T.BINOP (T.PLUS, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="add `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* MINUS *)
            |   munchExp (T.BINOP (T.MINUS, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 - c2))
            |   munchExp (T.BINOP (T.MINUS, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="addi `d0, `s0, " ^ Int.toString (~c) ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.MINUS, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sub `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* MUL *)
            |   munchExp (T.BINOP (T.MUL, e, T.CONST 0)) = zero
            |   munchExp (T.BINOP (T.MUL, T.CONST 0, e)) = zero
            |   munchExp (T.BINOP (T.MUL, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 * c2))
            |   munchExp (T.BINOP (T.MUL, e, T.CONST 1)) = munchExp e
            |   munchExp (T.BINOP (T.MUL, T.CONST 1, e)) = munchExp e
            |   munchExp (T.BINOP (T.MUL, e, T.CONST (~1))) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.MUL, T.CONST (~1), e)) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.MUL, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="mult `s0, `s1\nmflo `d0\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* DIV *)
            |   munchExp (T.BINOP (T.DIV, T.CONST 0, e)) = zero
            |   munchExp (T.BINOP (T.DIV, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 div c2))
            |   munchExp (T.BINOP (T.DIV, e, T.CONST 1)) = munchExp e
            |   munchExp (T.BINOP (T.DIV, e, T.CONST (~1))) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.DIV, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="div `s0, `s1\nmflo `d0\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* CONST *)
            |   munchExp (T.CONST 0) = zero
            |   munchExp (T.CONST c) =
                result (fn r => emit (A.OPER {
                    assem="addi `d0, $zero, " ^ Int.toString c ^ "\n",
                    src=[], dst=[r], jump=NONE
                }))
            (* TEMP *)
            |   munchExp (T.TEMP t) = t
        in rev (!ilist)
        end
end

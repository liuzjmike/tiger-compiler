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
            fun boolToConst b = T.CONST (if b then 1 else 0)
            
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
                    assem="sw `s0, 0(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.TEMP t, e)) =
                emit (A.OPER {
                    assem="or `d0, `s0, $zero\n",
                    src=[munchExp e], dst=[t], jump=NONE
                })
            |   munchStm (T.MOVE _) =
                ErrorMsg.impossible "illegal move statement"
            (* EXP *)
            |   munchStm (T.EXP (T.CALL (T.NAME label, args))) =
                emit (A.OPER {
                    assem="jal " ^ Symbol.name label ^ "\n",
                    src=munchArgs args, dst=Frame.calldefs, jump=NONE
                })
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
            |   munchExp (T.BINOP (T.PLUS, e, T.CONST 0)) = munchExp e
            |   munchExp (T.BINOP (T.PLUS, T.CONST 0, e)) = munchExp e
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
            |   munchExp (T.BINOP (T.MINUS, e, T.CONST 0)) = munchExp e
            |   munchExp (T.BINOP (T.MINUS, T.CONST 0, e)) =
                result (fn r => emit (A.OPER {
                    assem="neg `d0, `s0\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
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
                    assem="mul `d0, `s0, `s1\n",
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
                    assem="div d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* EQ *)
            |   munchExp (T.RELOP (T.EQ, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 = c2))
            |   munchExp (T.RELOP (T.EQ, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="seq `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* NE *)
            |   munchExp (T.RELOP (T.NE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 <> c2))
            |   munchExp (T.RELOP (T.NE, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sne `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* LT *)
            |   munchExp (T.RELOP (T.LT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 < c2))
            |   munchExp (T.RELOP (T.LT, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="slti `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.RELOP (T.LT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="slt `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* LE *)
            |   munchExp (T.RELOP (T.LE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 <= c2))
            |   munchExp (T.RELOP (T.LE, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sle `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* GT *)
            |   munchExp (T.RELOP (T.GT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 > c2))
            |   munchExp (T.RELOP (T.GT, T.CONST c, e)) =
                result (fn r => emit (A.OPER {
                    assem="slti `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.RELOP (T.GT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="slt `d0, `s0, `s1\n",
                    src=[munchExp e2, munchExp e1], dst=[r], jump=NONE
                }))
            (* GE *)
            |   munchExp (T.RELOP (T.GE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 >= c2))
            |   munchExp (T.RELOP (T.GE, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sge `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            |   munchExp (T.CALL (T.NAME label, args)) = (
                munchStm (T.EXP (T.CALL (T.NAME label, args)));
                Frame.RV
            )
            (* CONST *)
            |   munchExp (T.CONST 0) = zero
            |   munchExp (T.CONST c) =
                result (fn r => emit (A.OPER {
                    assem="ori `d0, $zero, " ^ Int.toString c ^ "\n",
                    src=[], dst=[r], jump=NONE
                }))
            (* TEMP *)
            |   munchExp (T.TEMP t) = t

            and munchArgs (args) =
                let fun f (i, [], argregs) = List.take (
                        Frame.argregs,
                        Int.min(i, List.length Frame.argregs)
                    )
                    |   f (i, a::l, []) = (
                        (* TODO: move arguments into the frame *)
                        (* munchStm (T.MOVE (T.MEM (), a)); *)
                        munchExp a;
                        f (i+1, l, [])
                    )
                    |   f (i, a::l, r::rs) = (
                        munchStm (T.MOVE (T.TEMP r, a));
                        f (i+1, l, rs)
                    )
                in f (0, args, Frame.argregs)
                end
        in rev (!ilist)
        end
end

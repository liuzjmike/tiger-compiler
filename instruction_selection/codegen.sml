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
                emit (A.MOVE {
                    assem="or `d0, `s0, $zero\n",
                    src=munchExp e, dst=t
                })
            |   munchStm (T.MOVE _) =
                ErrorMsg.impossible "illegal move statement"
            (* JUMP *)
            |   munchStm (T.JUMP (T.NAME label, labs)) =
                emit (A.OPER {
                    assem="j " ^ Symbol.name label ^ "\n",
                    src=[], dst=[], jump=SOME labs
                })
            |   munchStm (T.JUMP (e, labs)) =
                emit (A.OPER {
                    assem="jr `s0\n",
                    src=[munchExp e], dst=[], jump=SOME labs
                })
            (* CJUMP *)
            |   munchStm (T.CJUMP (T.CONST 0, t, f)) =
                munchStm (T.JUMP (T.NAME f, [f]))
            |   munchStm (T.CJUMP (T.CONST c, t, f)) =
                munchStm (T.JUMP (T.NAME t, [t]))
            |   munchStm (T.CJUMP (T.RELOP (T.EQ, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 = c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.EQ, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="beq `s0, `s1, " ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.NE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 <> c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.NE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bne `s0, `s1, " ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.LT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 < c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.LT, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="blt `s0, `s1" ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.LE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 <= c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.LE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="ble `s0, `s1" ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.GT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 > c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.GT, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bgt `s0, `s1" ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.GE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 >= c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.GE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bge `s0, `s1" ^ Symbol.name t ^ "\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.ULT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (Word.< (Word.fromInt c1, Word.fromInt c2)), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.ULE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (Word.<= (Word.fromInt c1, Word.fromInt c2)), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.UGT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (Word.> (Word.fromInt c1, Word.fromInt c2)), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.UGE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (Word.>= (Word.fromInt c1, Word.fromInt c2)), t, f))
            |   munchStm (T.CJUMP (e, t, f)) =
                emit (A.OPER {
                    assem="bne `s0, $zero, " ^ Symbol.name t ^ "\n",
                    src=[munchExp e], dst=[], jump=SOME [t, f]
                })
            (* EXP *)
            |   munchStm (T.EXP (T.CALL (T.NAME label, args))) =
                emit (A.OPER {
                    assem="jal " ^ Symbol.name label ^ "\n",
                    src=munchArgs args, dst=Frame.calldefs, jump=NONE
                })
            |   munchStm (T.EXP (T.CALL (e, args))) =
                emit (A.OPER {
                    assem="jal `s0\n",
                    src=(munchExp e)::munchArgs args, dst=Frame.calldefs, jump=NONE
                })
            (* FIXME: other special cases for EXP? *)
            |   munchStm (T.EXP e) = (munchExp e; ())
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
                munchExp (T.CONST (Int.quot (c1, c2)))
            |   munchExp (T.BINOP (T.DIV, e, T.CONST 1)) = munchExp e
            |   munchExp (T.BINOP (T.DIV, e, T.CONST (~1))) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.DIV, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="div `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* AND *)
            |   munchExp (T.BINOP (T.AND, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.andb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.AND, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="andi `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.AND, T.CONST c, e)) =
                munchExp (T.BINOP (T.AND, e, T.CONST c))
            |   munchExp (T.BINOP (T.AND, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="and `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* OR *)
            |   munchExp (T.BINOP (T.OR, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.orb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.OR, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="ori `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.OR, T.CONST c, e)) =
                munchExp (T.BINOP (T.OR, e, T.CONST c))
            |   munchExp (T.BINOP (T.OR, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="or `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* LSHIFT *)
            |   munchExp (T.BINOP (T.LSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.<< (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.LSHIFT, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="sll `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.LSHIFT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sllv `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* RSHIFT *)
            |   munchExp (T.BINOP (T.RSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.>> (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.RSHIFT, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="srl `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.RSHIFT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="srlv `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* ARSHIFT *)
            |   munchExp (T.BINOP (T.ARSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.~>> (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.ARSHIFT, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="sra `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.ARSHIFT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="srav `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* XOR *)
            |   munchExp (T.BINOP (T.XOR, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.xorb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.XOR, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="xori `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.BINOP (T.XOR, T.CONST c, e)) =
                munchExp (T.BINOP (T.XOR, e, T.CONST c))
            |   munchExp (T.BINOP (T.XOR, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="xor `d0, `s0, `s1\n",
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
            (* ULT *)
            |   munchExp (T.RELOP (T.ULT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.< (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.ULT, e, T.CONST c)) =
                result (fn r => emit (A.OPER {
                    assem="sltiu `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.RELOP (T.ULT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sltu `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* ULE *)
            |   munchExp (T.RELOP (T.ULE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.<= (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.ULE, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sleu `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* UGT *)
            |   munchExp (T.RELOP (T.UGT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.> (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.UGT, T.CONST c, e)) =
                result (fn r => emit (A.OPER {
                    assem="sltiu `d0, `s0, " ^ Int.toString c ^ "\n",
                    src=[munchExp e], dst=[r], jump=NONE
                }))
            |   munchExp (T.RELOP (T.UGT, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sltu `d0, `s0, `s1\n",
                    src=[munchExp e2, munchExp e1], dst=[r], jump=NONE
                }))
            (* UGE *)
            |   munchExp (T.RELOP (T.UGE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.>= (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.UGE, e1, e2)) =
                result (fn r => emit (A.OPER {
                    assem="sgeu `d0, `s0, `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }))
            (* CALL *)
            |   munchExp (T.CALL (e, args)) = (
                munchStm (T.EXP (T.CALL (e, args)));
                Frame.RV
            )
            (* NAME *)
            |   munchExp (T.NAME label) =
                result (fn r => emit (A.OPER {
                    assem="ori `d0, $zero, " ^ Symbol.name label ^ "\n",
                    src=[], dst=[r], jump=NONE
                }))
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

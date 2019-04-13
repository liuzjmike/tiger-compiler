structure MipsGen : CODEGEN =
struct
    structure A = Assem
    structure T = Tree

    structure Frame = MipsFrame

    fun codegen frame stm =
        let val ilist = ref []
            fun emit x = ilist := x :: !ilist
            fun result (assem, src) =
                let val t = Temp.newtemp ()
                in
                    emit (A.OPER {assem=assem, src=src, dst=[t], jump=NONE});
                    t
                end
            fun boolToConst b = T.CONST (if b then 1 else 0)
            fun intToString i = if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
            
            fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)
            (* MOVE *)
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2)), e)) =
                munchStm (T.MOVE (T.MEM (T.CONST (c1 + c2)), e))
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2)) =
                emit (A.OPER {
                    assem="sw `s0, " ^ intToString c ^ "(`s1)\n",
                    src=[munchExp e2, munchExp e1], dst=[], jump=NONE
                })
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST c, e1)), e2)) =
                munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST c)), e2))
            |   munchStm (T.MOVE (T.MEM (T.BINOP (T.MINUS, e1, T.CONST c)), e2)) =
                munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~c))), e2))
            |   munchStm (T.MOVE (T.MEM (T.CONST c), e)) = (
                if c < 0 then ErrorMsg.impossible "negative memory address" else ();
                emit (A.OPER {
                    assem="sw `s0, " ^ Int.toString c ^ "($zero)\n",
                    src=[munchExp e], dst=[], jump=NONE
                })
            )
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
                print "illegal move statement\n"
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
                    assem="beq `s0, `s1, `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.NE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 <> c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.NE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bne `s0, `s1, `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.LT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 < c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.LT, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="blt `s0, `s1, `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.LE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 <= c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.LE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="ble `s0, `s1, `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.GT, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 > c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.GT, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bgt `s0, `s1, `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=SOME [t, f]
                })
            |   munchStm (T.CJUMP (T.RELOP (T.GE, T.CONST c1, T.CONST c2), t, f)) =
                munchStm (T.CJUMP (boolToConst (c1 >= c2), t, f))
            |   munchStm (T.CJUMP (T.RELOP (T.GE, e1, e2), t, f)) =
                emit (A.OPER {
                    assem="bge `s0, `s1, `j0\n",
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
                    assem="bne `s0, $zero, `j0\n",
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
            |   munchStm (T.EXP e) = (munchExp e; ())
            (* LABEL *)
            |   munchStm (T.LABEL label) =
                emit (A.LABEL {assem=Symbol.name label ^ ":\n", lab=label})

            and munchExp (T.ESEQ (s, e)) = (munchStm s; munchExp e)
            (* MEM *)
            |   munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2))) =
                munchExp (T.MEM (T.CONST (c1 + c2)))
            |   munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST c))) =
                result ("lw `d0, " ^ intToString c ^ "(`s0)\n", [munchExp e])
            |   munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST c, e))) =
                munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST c)))
            |   munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST c))) =
                munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST (~c))))
            |   munchExp (T.MEM (T.CONST c)) = (
                if c < 0 then ErrorMsg.impossible "negative memory address" else ();
                result ("lw `d0, " ^ Int.toString c ^ "($zero)\n", [])
            )
            |   munchExp (T.MEM e) =
                result ("lw `d0, 0(`s0)\n", [munchExp e])
            (* PLUS *)
            |   munchExp (T.BINOP (T.PLUS, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 + c2))
            |   munchExp (T.BINOP (T.PLUS, e, T.CONST 0)) = munchExp e
            |   munchExp (T.BINOP (T.PLUS, T.CONST 0, e)) = munchExp e
            |   munchExp (T.BINOP (T.PLUS, e, T.CONST c)) =
                result ("addi `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.PLUS, T.CONST c, e)) =
                munchExp (T.BINOP (T.PLUS, e, T.CONST c))
            |   munchExp (T.BINOP (T.PLUS, e1, e2)) =
                result ("add `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* MINUS *)
            |   munchExp (T.BINOP (T.MINUS, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 - c2))
            |   munchExp (T.BINOP (T.MINUS, e, T.CONST 0)) = munchExp e
            |   munchExp (T.BINOP (T.MINUS, T.CONST 0, e)) =
                result ("neg `d0, `s0\n", [munchExp e])
            |   munchExp (T.BINOP (T.MINUS, e, T.CONST c)) =
                munchExp (T.BINOP (T.PLUS, e, T.CONST (~c)))
            |   munchExp (T.BINOP (T.MINUS, e1, e2)) =
                result ("sub `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* MUL *)
            |   munchExp (T.BINOP (T.MUL, e, T.CONST 0)) = Frame.ZERO
            |   munchExp (T.BINOP (T.MUL, T.CONST 0, e)) = Frame.ZERO
            |   munchExp (T.BINOP (T.MUL, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (c1 * c2))
            |   munchExp (T.BINOP (T.MUL, e, T.CONST 1)) = munchExp e
            |   munchExp (T.BINOP (T.MUL, T.CONST 1, e)) = munchExp e
            |   munchExp (T.BINOP (T.MUL, e, T.CONST (~1))) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.MUL, T.CONST (~1), e)) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.MUL, e1, e2)) =
                result ("mul `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* DIV *)
            |   munchExp (T.BINOP (T.DIV, T.CONST 0, e)) = Frame.ZERO
            |   munchExp (T.BINOP (T.DIV, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Int.quot (c1, c2)))
            |   munchExp (T.BINOP (T.DIV, e, T.CONST 1)) = munchExp e
            |   munchExp (T.BINOP (T.DIV, e, T.CONST (~1))) =
                munchExp (T.BINOP (T.MINUS, T.CONST 0, e))
            |   munchExp (T.BINOP (T.DIV, e1, e2)) =
                result ("div `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* AND *)
            |   munchExp (T.BINOP (T.AND, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.andb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.AND, e, T.CONST c)) =
                result ("andi `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.AND, T.CONST c, e)) =
                munchExp (T.BINOP (T.AND, e, T.CONST c))
            |   munchExp (T.BINOP (T.AND, e1, e2)) =
                result ("and `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* OR *)
            |   munchExp (T.BINOP (T.OR, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.orb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.OR, e, T.CONST c)) =
                result ("ori `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.OR, T.CONST c, e)) =
                munchExp (T.BINOP (T.OR, e, T.CONST c))
            |   munchExp (T.BINOP (T.OR, e1, e2)) =
                result ("or `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* LSHIFT *)
            |   munchExp (T.BINOP (T.LSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.<< (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.LSHIFT, e, T.CONST c)) =
                result ("sll `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.LSHIFT, e1, e2)) =
                result ("sllv `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* RSHIFT *)
            |   munchExp (T.BINOP (T.RSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.>> (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.RSHIFT, e, T.CONST c)) =
                result ("srl `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.RSHIFT, e1, e2)) =
                result ("srlv `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* ARSHIFT *)
            |   munchExp (T.BINOP (T.ARSHIFT, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.~>> (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.ARSHIFT, e, T.CONST c)) =
                result ("sra `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.ARSHIFT, e1, e2)) =
                result ("srav `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* XOR *)
            |   munchExp (T.BINOP (T.XOR, T.CONST c1, T.CONST c2)) =
                munchExp (T.CONST (Word.toInt (Word.xorb (Word.fromInt c1, Word.fromInt c2))))
            |   munchExp (T.BINOP (T.XOR, e, T.CONST c)) =
                result ("xori `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.BINOP (T.XOR, T.CONST c, e)) =
                munchExp (T.BINOP (T.XOR, e, T.CONST c))
            |   munchExp (T.BINOP (T.XOR, e1, e2)) =
                result ("xor `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* EQ *)
            |   munchExp (T.RELOP (T.EQ, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 = c2))
            |   munchExp (T.RELOP (T.EQ, e1, e2)) =
                result ("seq `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* NE *)
            |   munchExp (T.RELOP (T.NE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 <> c2))
            |   munchExp (T.RELOP (T.NE, e1, e2)) =
                result ("sne `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* LT *)
            |   munchExp (T.RELOP (T.LT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 < c2))
            |   munchExp (T.RELOP (T.LT, e, T.CONST c)) =
                result ("slti `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.RELOP (T.LT, e1, e2)) =
                result ("slt `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* LE *)
            |   munchExp (T.RELOP (T.LE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 <= c2))
            |   munchExp (T.RELOP (T.LE, e1, e2)) =
                result ("sle `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* GT *)
            |   munchExp (T.RELOP (T.GT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 > c2))
            |   munchExp (T.RELOP (T.GT, T.CONST c, e)) =
                result ("slti `d0, `s0, " ^ intToString c ^ "\n", [munchExp e])
            |   munchExp (T.RELOP (T.GT, e1, e2)) =
                result ("slt `d0, `s0, `s1\n", [munchExp e2, munchExp e1])
            (* GE *)
            |   munchExp (T.RELOP (T.GE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (c1 >= c2))
            |   munchExp (T.RELOP (T.GE, e1, e2)) =
                result ("sge `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* ULT *)
            |   munchExp (T.RELOP (T.ULT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.< (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.ULT, e, T.CONST c)) =
                result (
                    "sltiu `d0, `s0, " ^ Word.fmt StringCvt.DEC (Word.fromInt c) ^ "\n",
                    [munchExp e]
                )
            |   munchExp (T.RELOP (T.ULT, e1, e2)) =
                result ("sltu `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* ULE *)
            |   munchExp (T.RELOP (T.ULE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.<= (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.ULE, e1, e2)) =
                result ("sleu `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* UGT *)
            |   munchExp (T.RELOP (T.UGT, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.> (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.UGT, T.CONST c, e)) =
                result (
                    "sltiu `d0, `s0, " ^ Word.fmt StringCvt.DEC (Word.fromInt c) ^ "\n",
                    [munchExp e]
                )
            |   munchExp (T.RELOP (T.UGT, e1, e2)) =
                result ("sltu `d0, `s0, `s1\n", [munchExp e2, munchExp e1])
            (* UGE *)
            |   munchExp (T.RELOP (T.UGE, T.CONST c1, T.CONST c2)) =
                munchExp (boolToConst (Word.>= (Word.fromInt c1, Word.fromInt c2)))
            |   munchExp (T.RELOP (T.UGE, e1, e2)) =
                result ("sgeu `d0, `s0, `s1\n", [munchExp e1, munchExp e2])
            (* CALL *)
            |   munchExp (T.CALL (e, args)) = (
                munchStm (T.EXP (T.CALL (e, args)));
                Frame.RV
            )
            (* NAME *)
            |   munchExp (T.NAME label) =
                result ("ori `d0, $zero, " ^ Symbol.name label ^ "\n", [])
            (* CONST *)
            |   munchExp (T.CONST 0) = Frame.ZERO
            |   munchExp (T.CONST c) =
                result ("ori `d0, $zero, " ^ intToString c ^ "\n", [])
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
        in
            munchStm stm;
            rev (!ilist)
        end
end

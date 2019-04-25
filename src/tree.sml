structure Tree : TREE =
struct
  type label = Temp.label
  type size = int

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of exp * label * label
               | MOVE of exp * exp
               | EXP of exp

  and exp = BINOP of binop * exp * exp
          | RELOP of relop * exp * exp
          | MEM of exp
          | TEMP of Temp.temp
          | ESEQ of stm * exp
          | NAME of label
          | CONST of int
          | CALL of exp * exp list

  and binop = PLUS | MINUS | MUL | DIV
            | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

  and relop = EQ | NE | LT | GT | LE | GE
            | ULT | ULE | UGT | UGE

  fun seq [] = ErrorMsg.impossible "build statement from empty statement list"
    | seq [stm] = stm
    | seq (stm::l) = SEQ (stm, seq l)
  
  fun mem (a, offset) = MEM (BINOP (PLUS, a, offset))

  fun notRel (RELOP (relop, left, right)) =
      let 
        fun f EQ = NE
          | f NE = EQ
          | f LT = GE
          | f LE = GT
          | f GT = LE
          | f GE = LT
          | f ULT = UGE
          | f ULE = UGT
          | f UGT = ULE
          | f UGE = ULT
      in RELOP (f relop, left, right)
      end
    | notRel e = RELOP (EQ, e, CONST 0)


end

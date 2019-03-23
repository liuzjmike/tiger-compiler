structure Tree : TREE =
struct
  type label = Temp.label
  type size = int

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
               | MOVE of lvalue * exp
               | EXP of exp

  and exp = BINOP of binop * exp * exp
          | LVALUE of lvalue
          | ESEQ of stm * exp
          | NAME of label
          | CONST of int
          | CALL of exp * exp list

  and lvalue = MEM of exp
             | TEMP of Temp.temp

  and binop = PLUS | MINUS | MUL | DIV
            | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

  and relop = EQ | NE | LT | GT | LE | GE
            | ULT | ULE | UGT | UGE

  fun seq [] = ErrorMsg.impossible "build statement from empty statement list"
    | seq [stm] = stm
    | seq (stm::l) = SEQ (stm, seq l)

end

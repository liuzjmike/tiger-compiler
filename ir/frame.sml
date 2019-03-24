structure MipsFrame : FRAME =
struct
    structure T = Tree

    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list, nLocal: int ref}

    val wordSize = 4 (* 32 bit *)
    val nArgReg = 4

    fun newLocal true count = (count := !count + 1; InFrame (~wordSize * !count))
    |   newLocal false count = InReg (Temp.newtemp ())

    fun newFrame {name, formals} =
        let val nLocal = ref 0
            fun f escape = newLocal escape nLocal
        in {name=name, formals=map f formals, nLocal=nLocal}
        end

    fun name {name, formals, nLocal} = name

    fun formals {name, formals, nLocal} = formals

    fun allocLocal {name, formals, nLocal} escape = newLocal escape nLocal

    val FP = Temp.newtemp ()

    fun exp (InReg r) frameAddr = T.TEMP r
    |   exp (InFrame k) frameAddr = T.MEM (T.BINOP (T.PLUS, frameAddr, T.CONST k))

    val RV = Temp.newtemp ()

    fun externalCall (f, args) = T.CALL (T.NAME (Temp.namedlabel f), args)

    (* TODO: Special treatment calls with more than 4 arguments *)
    fun procEntryExit1 (frame, body) = body

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

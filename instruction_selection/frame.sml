structure MipsFrame : FRAME =
struct
    structure T = Tree

    val FP = Temp.newtemp ()
    val RV = Temp.newtemp ()
    val wordSize = 4 (* 32 bit *)

    fun externalCall (f, args) = T.CALL (T.NAME (Temp.namedlabel f), args)

    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list, nLocal: int ref}

    fun newLocal true count = (count := !count + 1; InFrame (~wordSize * !count))
    |   newLocal false count = InReg (Temp.newtemp ())

    fun newFrame {name, formals} =
        let val nLocal = ref 0
            fun f escape = newLocal escape nLocal
        in {name=name, formals=map f formals, nLocal=nLocal}
        end

    fun name {name, formals, nLocal} = Symbol.name name

    fun formals {name, formals, nLocal} = formals

    fun allocLocal {name, formals, nLocal} escape = newLocal escape nLocal

    fun string (label, s) = s

    fun exp (InReg r) frameAddr = T.TEMP r
    |   exp (InFrame k) frameAddr = T.mem (frameAddr, T.CONST k)

    (* TODO: Special treatment calls with more than 4 arguments *)
    fun procEntryExit1 (frame, body) = body

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

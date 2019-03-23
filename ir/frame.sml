structure MipsFrame : FRAME =
struct
    structure T = Tree

    datatype access = InFrame of int
                    | InReg of Temp.temp
    (* TODO: Add prolog instructions and move args to the right place *)
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

    (* val externalCall: string * Tree.exp list -> Tree.exp
    val procEntryExit1: frame * Tree.stm -> Tree.stm *)

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

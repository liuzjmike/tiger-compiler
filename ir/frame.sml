structure MipsFrame : FRAME =
struct
    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list, nLocal: int ref}

    val wordSize = 4 (* 32 bit *)
    val nArgReg = 4

    fun newLocal true count =
        let val res = InFrame (!count)
        in
            count := !count + 1; (* FIXME: may want to increment by a different number such as wordSize *)
            res
        end
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
    val RV = Temp.newtemp ()
    (* val exp: access -> Tree.exp -> Tree.exp *)

    (* val externalCall: string * Tree.exp list -> Tree.exp
    val procEntryExit1: frame * Tree.stm -> Tree.stm *)

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

structure Frame : FRAME = MipsFrame

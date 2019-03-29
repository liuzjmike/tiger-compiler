structure MipsFrame : FRAME =
struct
    structure T = Tree

    type register = string
    val FP = Temp.newtemp ()
    val RV = Temp.newtemp ()
    val specialregs = [
        ("$zero", Temp.newtemp ()),
        ("$at", Temp.newtemp ()),
        ("$v0", RV),
        ("$v1", Temp.newtemp ()),
        ("$k0", Temp.newtemp ()),
        ("$k1", Temp.newtemp ()),
        ("$gp", Temp.newtemp ()),
        ("$sp", Temp.newtemp ()),
        ("$fp", FP),
        ("$ra", Temp.newtemp ())
    ]
    val argregs = [
        ("$a0", Temp.newtemp ()),
        ("$a1", Temp.newtemp ()),
        ("$a2", Temp.newtemp ()),
        ("$a3", Temp.newtemp ())
    ]
    val calleesaves = [
        ("$t0", Temp.newtemp ()),
        ("$t1", Temp.newtemp ()),
        ("$t2", Temp.newtemp ()),
        ("$t3", Temp.newtemp ()),
        ("$t4", Temp.newtemp ()),
        ("$t5", Temp.newtemp ()),
        ("$t6", Temp.newtemp ()),
        ("$t7", Temp.newtemp ()),
        ("$t8", Temp.newtemp ()),
        ("$t9", Temp.newtemp ())
    ]
    val callersaves = [
        ("$s0", Temp.newtemp ()),
        ("$s1", Temp.newtemp ()),
        ("$s2", Temp.newtemp ()),
        ("$s3", Temp.newtemp ()),
        ("$s4", Temp.newtemp ()),
        ("$s5", Temp.newtemp ()),
        ("$s6", Temp.newtemp ()),
        ("$s7", Temp.newtemp ())
    ]
    val tempMap =
        foldl (fn (l, m) => 
            foldl (fn ((name, t), m) => Temp.Table.enter (m, t, name)) m l)
        Temp.Table.empty
        [specialregs, argregs, calleesaves, callersaves]

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

structure MipsFrame :> FRAME =
struct
    structure T = Tree

    type register = string
    val ZERO = Temp.newtemp ()
    val RV = Temp.newtemp ()
    val SP = Temp.newtemp ()
    val FP = Temp.newtemp ()
    val RA = Temp.newtemp ()
    val specialregs = [
        ("$zero", ZERO),
        ("$sp", SP),
        ("$fp", FP),
        ("$ra", RA)
    ]
    val argregs' = [
        ("$a0", Temp.newtemp ()),
        ("$a1", Temp.newtemp ()),
        ("$a2", Temp.newtemp ()),
        ("$a3", Temp.newtemp ())
    ]
    val calleesaves = [
        ("$s0", Temp.newtemp ()),
        ("$s1", Temp.newtemp ()),
        ("$s2", Temp.newtemp ()),
        ("$s3", Temp.newtemp ()),
        ("$s4", Temp.newtemp ()),
        ("$s5", Temp.newtemp ()),
        ("$s6", Temp.newtemp ()),
        ("$s7", Temp.newtemp ())
    ]
    val callersaves = [
        ("$v0", RV),
        ("$v1", Temp.newtemp ()),
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
    val argregs = map #2 argregs'
    val calldefs = RA :: argregs @ (map #2 callersaves)
    val tempMap =
        foldl (fn (l, m) => 
            foldl (fn ((name, t), m) => Temp.Map.insert (m, t, name)) m l)
        Temp.Map.empty
        [specialregs, argregs', calleesaves, callersaves]
    val registers = Temp.Map.listItems tempMap

    val wordSize = 4 (* 32 bit *)
    val align = ".align " ^ Int.toString wordSize ^ "\n"

    fun externalCall (f, args) = T.CALL (T.NAME (Temp.namedlabel f), args)

    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list, nLocal: int ref, nArg: int ref}

    fun newLocal true count = (count := !count + 1; InFrame (~wordSize * !count))
    |   newLocal false count = InReg (Temp.newtemp ())

    fun newFrame {name, formals} =
        let val nLocal = ref 0
            fun f escape = newLocal escape nLocal
        in {name=name, formals=map f formals, nLocal=nLocal, nArg=ref 0}
        end

    fun name {name, formals, nLocal, nArg} = Symbol.name name

    fun formals {name, formals, nLocal, nArg} = formals

    fun allocLocal {name, formals, nLocal, nArg} escape = newLocal escape nLocal

    fun allocArgs ({name, formals, nLocal, nArg}, n) = nArg := Int.max (!nArg, n)

    fun string (label, s) =
        ".data\n" ^ align
        ^ Symbol.name label ^ ":\n"
        ^ ".word " ^ Int.toString (String.size s) ^ "\n"
        ^ ".ascii \"" ^ String.toString s ^ "\"\n\n"

    fun exp (InReg r) frameAddr = T.TEMP r
    |   exp (InFrame k) frameAddr = T.mem (frameAddr, T.CONST k)

    fun foldCallerSave ((r, t), (entryMoves, exitMoves)) =
        let val temp = Temp.newtemp ()
        in (
            T.MOVE (T.TEMP temp, T.TEMP t) :: entryMoves,
            T.MOVE (T.TEMP t, T.TEMP temp) :: exitMoves
        )
        end
    val (entryMoves, exitMoves) = foldl foldCallerSave ([], []) (("$ra", RA)::calleesaves)
    val entryMoves = T.seq (List.rev entryMoves)
    val exitMoves = T.seq (List.rev exitMoves)
    fun procEntryExit1 ({name, formals, nLocal, nArg}, body) =
        (* TODO: don't save static link unless necessary *)
        let fun moveActuals (_, [], n) = []
            |   moveActuals ([], a::formals, n) =
                let val move = T.MOVE (
                        exp a (T.TEMP FP),
                        T.mem (T.TEMP FP, T.CONST n)
                    )
                in move :: moveActuals ([], formals, n + wordSize)
                end
            |   moveActuals (r::argregs, a::formals, n) =
                T.MOVE (exp a (T.TEMP FP), T.TEMP r) :: moveActuals (argregs, formals, n)
        in T.seq [
            entryMoves,
            T.seq (moveActuals (argregs, formals, 0)),
            body,
            exitMoves
        ]
        end

    fun procEntryExit2 (frame, body) =
        body @
        [Assem.OPER {
            assem="", dst=[], jump=SOME [],
            src=map #2 (specialregs @ calleesaves)
        }]
    fun procEntryExit3 ({name, formals, nLocal, nArg}, body) = 
    let val name = Symbol.name name
        val fpOffset = (!nLocal + 1) * wordSize
        val fpOffset' = Int.toString fpOffset
        val frameSize = fpOffset + Int.max (!nArg - 4, 0) * 4
        val frameSize' = Int.toString frameSize
    in {
        prolog=
            ".text\n"
            ^ (if name = "tig_main" then ".globl tig_main\n" else "")
            ^ name ^ ":\n"
            ^ "sw $fp, -" ^ fpOffset' ^ "($sp)\n"
            ^ "or $fp, $sp, $zero\n"
            ^ "addi $sp, $sp, -" ^ frameSize' ^ "\n",
        body=body,
        epilog=
            "or $sp, $fp, $zero\n"
            ^ "lw $fp, -" ^ fpOffset' ^ "($fp)\n"
            ^ "jr $ra\n"
            ^ ".end " ^ name ^ "\n\n"
    }
    end

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

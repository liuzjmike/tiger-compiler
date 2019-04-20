structure MipsFrame : FRAME =
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

    fun string (label, s) = Symbol.name label ^ ": .asciiz \"" ^ String.toString s ^ "\"\n"

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
    (* TODO: special treatment calls with more than 4 arguments *)
    fun procEntryExit1 ({name, formals, nLocal}, body) =
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
            T.LABEL name,
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
    fun procEntryExit3 ({name, formals, nLocal}, body) = {
        prolog="PROCEDURE " ^ Symbol.name name ^ "\n",
        body=body,
        epilog="END " ^ Symbol.name name ^ "\n"
    }

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end

structure MipsFrame : FRAME = struct
    datatype access = memoryAddr of int | register of Temp.temp
    type frame = {label: Temp.label, formals: access list, frameLocalCount: int ref}
    datatype frag = PROC of {body: Tree.stm, frame: frame} | STRING of Temp.label * string

	val RV = Temp.newtemp()
	val FP = Temp.newtemp()
    (* 32bit system *)
	val wordSize = 4
    (* Number of args that are passed in registers for MIPS. *)
	val numArgRegs = 4

	fun name {label, formals, frameLocalCount} = label

    fun formals {label, formals, frameLocalCount} = formals

	(* fun newFrame {name, formals} =

		end *)

end


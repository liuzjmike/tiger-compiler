structure Main =
struct
    structure MipsSemant = Semant (Translate (MipsFrame))

    fun main filename =
        let val ast = Parse.parse filename
        in
            FindEscape.findescape ast;
            MipsSemant.transProg ast
        end

end
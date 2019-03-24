structure Main =
struct
    structure MipsSemant = Semant (Translate (MipsFrame))

    fun main filename =
        let val ast = Parse.parse filename
        in
            FindEscape.findEscape ast;
            MipsSemant.transProg ast
        end

end
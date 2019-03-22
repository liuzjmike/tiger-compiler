structure Main =
struct
    structure MipsSemant = Semant (Translate (MipsFrame))

    fun main filename = MipsSemant.transProg (Parse.parse filename)
end
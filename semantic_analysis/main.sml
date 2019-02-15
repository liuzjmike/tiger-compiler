structure Main =
struct
    fun main filename = Semant.transProg (Parse.parse filename)
end
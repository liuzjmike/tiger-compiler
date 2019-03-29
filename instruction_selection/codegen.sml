structure MipsGen : CODEGEN =
struct
    structure A = Assem

    structure Frame = MipsFrame

    fun codegen frame stm = []
end
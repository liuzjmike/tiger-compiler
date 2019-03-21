structure MipsFrame =
struct
    (* TODO *)
    datatype access = InFrame of int
                    | InReg of Temp.temp
end

structure Frame = MipsFrame

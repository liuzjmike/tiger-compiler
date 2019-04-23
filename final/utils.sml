structure Utils =
struct
  fun intToString i = if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
end

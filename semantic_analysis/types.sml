structure Types =
struct

  type unique = unit ref

  datatype ty = 
    RECORD of (Symbol.symbol * (unit -> ty)) list * unique
  | NIL
  | INT
  | STRING
  | ARRAY of (unit -> ty) * unique
  | UNIT
  | BOTTOM

end

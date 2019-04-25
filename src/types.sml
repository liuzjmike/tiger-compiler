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

  fun isSubtype (sub, super) =
    case (sub, super) of
      (RECORD (_, u1), RECORD (_, u2)) => u1 = u2
    | (NIL, RECORD _) => true
    | (NIL, NIL) => true
    | (INT, INT) => true
    | (STRING, STRING) => true
    | (ARRAY (_, u1), ARRAY (_, u2)) => u1 = u2
    | (UNIT, UNIT) => true
    | (BOTTOM, _) => true
    | _ => false
  
  fun isBottom ty =
    case ty of
      BOTTOM => true
    | _ => false

end

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
    | (NIL, NIL) => true (* TODO: we might want to return false or raise error here
                            because comparing two nil's is illegal and this branch
                            should not be hit in any other situation *)
    | (INT, INT) => true
    | (STRING, STRING) => true
    | (ARRAY (_, u1), ARRAY (_, u2)) => u1 = u2
    | (NIL, ARRAY _) => true
    | (UNIT, UNIT) => true
    | (BOTTOM, _) => true
    | _ => false
  
  fun isBottom ty =
    case ty of
      BOTTOM => true
    | _ => false

end

signature TRANSLATE = sig
    type exp
    type level
    type access (* not the same as Frame.access *)
    type frag

    val outermost: level

    val newLevel: {parent: level, name: Temp.label,
                   formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
    val procEntryExit: {level: level , body: exp} -> unit
    val getResult: unit -> frag list

    val nilExp: unit -> exp
    val intExp: int -> exp
    val stringExp: string -> exp
    val callExp: exp list * level * level * Temp.label -> exp
    val plusExp: exp * exp -> exp
    val minusExp: exp * exp -> exp
    val mulExp: exp * exp -> exp
    val divExp: exp * exp -> exp
    val eqExp: exp * exp * bool -> exp
    val neExp: exp * exp * bool -> exp
    val ltExp: exp * exp * bool -> exp
    val leExp: exp * exp * bool -> exp
    val gtExp: exp * exp * bool -> exp
    val geExp: exp * exp * bool -> exp
    val recordExp: exp list -> exp
    val seqExp: exp list -> exp
    val assignExp: exp * exp -> exp
    val ifThenExp: exp * exp -> exp
    val ifThenElseExp: exp * exp * exp -> exp
    val whileExp: exp * (Temp.label -> exp) -> exp
    val forExp: access * exp * exp * (Temp.label -> exp) -> exp
    val breakExp: Temp.label -> exp
    val letExp: exp list * exp -> exp
    val arrayExp: exp * exp -> exp
    val simpleVar: access * level -> exp
    val fieldVar: exp * int -> exp
    val subscriptVar: exp * exp -> exp
    val varDec: access * exp -> exp
end

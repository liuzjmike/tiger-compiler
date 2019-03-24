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

    val simpleVar: access * level -> exp
    val subscriptVar: exp * exp -> exp
    val nilExp: unit -> exp
    val intExp: int -> exp
    val stringExp: string -> exp
    val callExp: exp list * level * level * Temp.label -> exp
    val plusExp: exp * exp -> exp
    val minusExp: exp * exp -> exp
    val mulExp: exp * exp -> exp
    val divExp: exp * exp -> exp
    val eqExp: exp * exp -> exp
    val neExp: exp * exp -> exp
    val ltExp: exp * exp -> exp
    val leExp: exp * exp -> exp
    val gtExp: exp * exp -> exp
    val geExp: exp * exp -> exp
    val recordExp: exp list -> exp
    val seqExp: exp list -> exp
    val assignExp: exp * exp -> exp
    val ifThenExp: exp * exp -> exp
    val ifThenElseExp: exp * exp * exp -> exp
    val whileExp: exp * exp -> exp * Temp.label
    val forExp: access * exp * exp * exp -> exp * Temp.label
    val breakExp: Temp.label -> exp
    val arrayExp: exp * exp -> exp
    val varDec: access * exp -> exp
end

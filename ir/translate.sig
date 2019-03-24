signature TRANSLATE = sig
    type exp
    type level
    type access (* not the same as Frame.access *)

    val outermost: level

    val newLevel: {parent: level, name: Temp.label,
                   formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
    val simpleVar: access * level -> exp
    val procEntryExit: {level: level , body: exp} -> unit
    structure Frame : FRAME
    val getResult: unit -> Frame.frag list
end

(* TODO *)
structure FindEscape:
    sig val findEscape: Absyn.exp -> unit end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table
    fun traverseVar(env:escEnv, d:depth, s:Absyn.var) = ()
    and traverseExp(env:escEnv, drdepth, s:Absyn.exp) = ()
    and traverseDecs(env, d, s: Absyn.dec list) = Symbol.empty
    fun findEscape(prog: Absyn.exp) = ()
end

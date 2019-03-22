structure FindEscape:
    sig val findEscape: Absyn.exp -> unit end =
struct
    structure A = Absyn
    structure S = Symbol

    type depth = int
    type escEnv = (depth * bool ref) Symbol.table
    fun traverseVar(env, d, var) =
        let fun trvar (A.SimpleVar (id, pos)) = (
                case S.look (env, id)
                of  SOME (d', escape) => if d > d' then escape := true else ()
                |   NONE => ()
            )
            |   trvar (A.FieldVar (lvalue, id, pos)) = trvar lvalue
            |   trvar (A.SubscriptVar (lvalue, exp, pos)) = (trvar lvalue; traverseExp (env, d, exp))
        in trvar var
        end

    and traverseExp(env, d, exp) = 
        let fun trexp (A.VarExp var) = traverseVar(env, d, var)
            |   trexp (A.CallExp {func, args, pos}) = (map trexp args; ())
            |   trexp (A.OpExp {left, oper=_, right, pos}) = (trexp left; trexp right; ())
            |   trexp (A.RecordExp {fields, typ, pos}) =
                let fun f (id, exp, pos) = trexp exp
                in map f fields; ()
                end
            |   trexp (A.SeqExp expList) =
                let fun f (exp, pos) = trexp exp
                in map f expList; ()
                end
            |   trexp (A.AssignExp {var, exp, pos}) = (traverseVar (env, d, var); trexp exp)
            |   trexp (A.IfExp {test, then', else', pos}) = (
                trexp test;
                trexp then';
                case else'
                of  SOME e => trexp e
                |   NONE => ()
            )
            |   trexp (A.WhileExp {test, body, pos}) = (trexp test; trexp body)
            |   trexp (A.ForExp {var, escape, lo, hi, body, pos}) = (
                    trexp lo;
                    trexp hi;
                    traverseExp (S.enter (env, var, (d, escape)), d, body)
                )
            |   trexp (A.LetExp {decs, body, pos}) = traverseExp (traverseDecs (env, d, decs), d, body)
            |   trexp (A.ArrayExp {typ, size, init, pos}) = trexp init
            |   trexp _ = ()
        in trexp exp
        end

    and traverseDecs(env, d, decs) =
        let fun trdec (A.FunctionDec decList, env) =
            let val d = d + 1
                fun trfundec {name, params, result, body, pos} =
                    let fun enterparam ({name, escape, typ, pos}, env) = S.enter (env, name, (d, escape))
                    in traverseExp (foldl enterparam env params, d, body)
                    end
            in
                map trfundec decList;
                env
            end
            |   trdec (A.VarDec {name, escape, typ=NONE, init, pos}, env) = (
                traverseExp (env, d, init);
                S.enter (env, name, (d, escape))
            )
            |   trdec _ = env
        in 
            foldl trdec env decs
        end
    fun findEscape(prog) = traverseExp (S.empty, 0, prog)
end

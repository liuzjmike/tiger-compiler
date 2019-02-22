structure Semant:
    sig val transProg: Absyn.exp -> Translate.exp end =
struct

    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Types
    val error = ErrorMsg.error

    fun transProg exp =
        let fun checkInt({exp, ty}, pos) =
                case ty
                    of  T.INT => exp
                    |   _ => (error pos "integer required"; ())

            fun transExp (venv, tenv, inLoop, exp) =
                let fun trexp (A.VarExp var) = trvar var
                    |   trexp A.NilExp = {exp=(), ty=T.NIL}
                    |   trexp (A.IntExp i) = {exp=(), ty=T.INT}
                    |   trexp (A.StringExp (s, pos)) = {exp=(), ty=T.STRING}
                    |   trexp (A.CallExp {func, args, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.OpExp {left, oper=A.PlusOp, right, pos}) = (
                        checkInt (trexp left, pos);
                        checkInt (trexp right, pos);
                        {exp=(), ty=T.INT}
                    )
                    |   trexp (A.OpExp {left, oper=A.MinusOp, right, pos}) = (
                        checkInt (trexp left, pos);
                        checkInt (trexp right, pos);
                        {exp=(), ty=T.INT}
                    )
                    |   trexp (A.OpExp {left, oper=A.TimesOp, right, pos}) = (
                        checkInt (trexp left, pos);
                        checkInt (trexp right, pos);
                        {exp=(), ty=T.INT}
                    )
                    |   trexp (A.OpExp {left, oper=A.DivideOp, right, pos}) = (
                        checkInt (trexp left, pos);
                        checkInt (trexp right, pos);
                        {exp=(), ty=T.INT}
                    )
                    (* TODO: Comparison operators *)
                    |   trexp (A.RecordExp {fields, typ, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.SeqExp expList) =
                        let fun f [(exp, pos)] = let val {exp=res, ty=ty} = trexp exp in ([res], ty) end
                            |   f ((exp, pos)::l) =
                                let val (resList, ty) = f l
                                in ((#exp (trexp exp))::resList, ty)
                                end
                            val (resList, ty) = f expList
                        in {exp=(), ty=ty}
                        end
                    |   trexp (A.AssignExp {var, exp, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.IfExp {test, then', else', pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.WhileExp {test, body, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.ForExp {var, escape, lo, hi, body, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.BreakExp pos) =
                        if inLoop
                        then {exp=(), ty=T.BOTTOM}
                        else (error pos "break outside of loop"; {exp=(), ty=T.BOTTOM})
                    |   trexp (A.LetExp {decs, body, pos}) =
                        let val {venv=venv', tenv=tenv'} = transDecs (venv, tenv, decs)
                        in transExp (venv', tenv', inLoop, body)
                        end
                    |   trexp (A.ArrayExp {typ, size, init, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)

                    and trvar (A.SimpleVar (id, pos)) = (
                        case S.look (venv, id)
                            of  SOME (E.VarEntry {ty, forIdx}) => {exp=(), ty=ty}
                            |   NONE => (
                                error pos ("undefined variable " ^ S.name id);
                                {exp=(), ty=T.BOTTOM}
                        )
                    )
                    |   trvar (A.FieldVar (lvalue, id, pos)) =
                        let val {exp=exp, ty=ty} = trvar lvalue
                            fun f [] = (error pos ("field " ^ S.name id ^ " does not exist"); {exp=(), ty=T.BOTTOM})
                            |   f ((id', ty')::l) = if id' = id then {exp=(), ty=ty'()} else f l
                        in case ty
                            of  T.RECORD (fieldList, unique) => f fieldList
                            |   T.BOTTOM => {exp=(), ty=T.BOTTOM}
                            |   _ => (error pos "access field of non-record type"; {exp=(), ty=T.BOTTOM})
                        end
                    |   trvar (A.SubscriptVar (lvalue, exp, pos)) =
                        let val {exp=lvExp, ty=ty} = trvar lvalue
                        in
                            case ty
                                of  T.ARRAY (eleTy, unique) =>
                                    let val {exp=idxExp, ty=idxTy} = trexp exp
                                    in case idxTy
                                        of  T.INT => {exp=(), ty=eleTy ()}
                                        |   _ => (error pos "index with non-int type"; {exp=(), ty=T.BOTTOM})
                                    end
                                |   T.BOTTOM => {exp=(), ty=T.BOTTOM}
                                |   _ => (error pos "index non-array type"; {exp=(), ty=T.BOTTOM})
                        end
                in
                    trexp exp
                end

            and transDecs (venv, tenv, decs) =
                let fun buildEnv (dec, {venv, tenv}) = transDec (venv, tenv, dec)
                in foldl buildEnv {venv=venv, tenv=tenv} decs
                end

            and transDec (venv, tenv, A.FunctionDec decList) = {venv=venv, tenv=tenv} (* TODO *)
            |   transDec (venv, tenv, A.VarDec {name, escape, typ, init, pos}) = {venv=venv, tenv=tenv} (* TODO *)
            |   transDec (venv, tenv, A.TypeDec decList) =
                let val decList = map (fn dec => (dec, ref ())) decList
                    fun addToEnv f (({name, ty, pos}, unique), t) = (
                        case S.look (t, name)
                            of  SOME _ => error pos (
                                    "duplicate type declaration "
                                    ^ S.name name
                                    ^ " in one mutually recursive group"
                                )
                            |   NONE => ();
                        S.enter (t, name, f (name, ty, unique))
                    )
                    val localTEnv = foldl (addToEnv (fn (name, ty, unique) => (ty, unique))) S.empty decList
                    fun getType (name, pos, seen) =
                        case S.look(localTEnv, name)
                            of  SOME (ty, unique) => transTy (name, ty, unique, seen)
                            |   NONE => case S.look(tenv, name)
                                            of  SOME ty => (fn () => ty)
                                            |   NONE => (error pos ("unbound type " ^ S.name name);
                                                         fn () => T.BOTTOM)
                    and transTy (name, A.NameTy (typ, pos), unique, seen) =
                        let val seen = S.enter (seen, name, ())
                        in
                            case S.look (seen, typ)
                                of  SOME _ => (error pos "circular type aliasing"; fn () => T.BOTTOM)
                                |   NONE => fn () => getType (typ, pos, seen) ()
                        end
                    |   transTy (name, A.RecordTy (fieldList), unique, seen
                        ) =
                        let fun buildFields ({name, escape, typ, pos}, l) = (name, getType (typ, pos, seen))::l
                        in fn () => T.RECORD (foldr buildFields nil fieldList, unique)
                        end
                    |   transTy (name, A.ArrayTy (typ, pos), unique, seen) =
                            (fn () => T.ARRAY (getType (typ, pos, seen), unique))
                in
                    {venv=venv, tenv=foldl (addToEnv (
                        fn (name, ty, unique) =>
                            transTy (name, ty, unique, S.empty) ()
                        )) tenv decList}
                end

        in
            #exp (transExp (E.base_venv, E.base_tenv, false, exp))
        end

end

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

            fun transExp (venv, tenv, exp) =
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
                    |   trexp exp = {exp=(), ty=T.BOTTOM} (* TODO *)

                    and trvar (A.SimpleVar (id, pos)) = (
                        case S.look (venv, id)
                        of  SOME (E.VarEntry {ty}) => {exp=(), ty=ty}
                        |   NONE => (
                            error pos ("undefined variable " ^ S.name id);
                            {exp=(), ty=T.BOTTOM}
                        )
                    )
                    |   trvar (var) = {exp=(), ty=T.BOTTOM} (* TODO *)
                in
                    trexp exp
                end

            and transDecs (venv, tenv, decs) =
                let fun buildEnv (dec, {venv, tenv}) = transDec (venv, tenv, dec)
                in foldl buildEnv {venv=venv, tenv=tenv} decs
                end

            and transDec (venv, tenv, A.TypeDec(decList)) =
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
                        |   NONE => case S.look(tenv, name) of
                                        SOME ty => (fn () => ty)
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
            |   transDec (venv, tenv, dec) = {venv=venv, tenv=tenv} (* TODO *)

        in
            #exp (transExp (E.base_venv, E.base_tenv, exp))
        end

end

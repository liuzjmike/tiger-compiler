structure A = Absyn
structure S = Symbol
structure T = Types

structure Semant = 
struct

    fun transProg exp = ()

    and transDecs (venv, tenv, decs) =
        let fun buildEnv (dec, {venv, tenv}) = transDec (venv, tenv, dec)
        in foldl buildEnv {venv=venv, tenv=tenv} decs
        end
    
    and transDec (venv, tenv, A.TypeDec(decList: {name: S.symbol, ty: A.ty, pos: A.pos} list)) =
        let val decList = map (fn dec => (dec, ref ())) decList
            fun addToEnv f (({name, ty, pos}, unique), t) = S.enter (t, name, f (ty, unique))
            val localTEnv = foldl (addToEnv (fn x => x)) S.empty decList
            fun getType (name, pos) = 
                case S.look(localTEnv, name) of
                    SOME (ty, unique) => transTy (getType, ty, unique)
                  | NONE => case S.look(tenv, name) of
                                SOME ty => (fn () => ty)
                              | NONE => (ErrorMsg.error pos ("unbound type " ^ S.name name);
                                         fn () => T.BOTTOM)
        in
            {venv=venv, tenv=foldl (addToEnv (fn (ty, unique) => transTy (getType, ty, unique) ())) tenv decList}
        end
      | transDec (venv, tenv, dec) = {venv=venv, tenv=tenv}
    
    and transTy (getType, A.NameTy (typ, pos), unique) = getType (typ, pos)
      | transTy (getType, A.RecordTy (fieldList: {name: S.symbol, escape: bool ref, typ: S.symbol, pos: A.pos} list), unique) =
        let fun buildFields ({name, escape, typ, pos}, l) = (name, getType (typ, pos))::l
        in fn () => T.RECORD (foldr buildFields nil fieldList, unique)
        end
      | transTy (getType, A.ArrayTy (typ, pos), unique) = (fn () => T.ARRAY (getType (typ, pos), unique))

end

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
        let fun addToEnv f ({name, ty, pos}, t) = S.enter (t, name, f ty)
            val localTEnv = foldl (addToEnv (fn x => x)) S.empty decList
            fun getType (name, pos): unit -> Types.ty = 
                case S.look(localTEnv, name) of
                    SOME ty => transTy (getType, ty)
                  | NONE => case S.look(tenv, name) of
                                SOME ty => (fn () => ty)
                              | NONE => (ErrorMsg.error pos ("unbound type " ^ S.name name);
                                         fn () => T.BOTTOM)
        in
            {venv=venv, tenv=foldl (addToEnv (fn x => transTy(getType, x) ())) tenv decList}
        end
      | transDec (venv, tenv, dec) = {venv=venv, tenv=tenv}
    
    and transTy (getType, A.NameTy(typ, pos)) = getType (typ, pos)
      | transTy (getType, A.RecordTy(fieldList: {name: S.symbol, escape: bool ref, typ: S.symbol, pos: A.pos} list)) =
        let fun buildFields ({name, escape, typ, pos}, l) = (name, getType (typ, pos))::l
            val unique = ref ()
        in fn () => T.RECORD (foldr buildFields nil fieldList, unique)
        end
      | transTy (getType, ty) = (fn () => T.NIL)

end

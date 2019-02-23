structure Semant:
    sig val transProg: Absyn.exp -> Translate.exp end =
struct

    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Types
    val error = ErrorMsg.error

    fun transProg exp =
        let fun unboundTypeError (ty, pos) = error pos ("unbound type " ^ S.name ty)

            fun transExp (venv, tenv, inLoop, exp) =
                let fun checkInt ({exp, ty}, pos) =
                        case ty
                            of  T.INT => exp
                            |   T.BOTTOM => exp
                            |   _ => (error pos "integer required"; ())

                    fun checkArithmeticOperands (expty1, expty2, pos) =
                        (checkInt (expty1, pos), checkInt (expty2, pos))
                    
                    fun checkComparisonOperands ({exp=exp1, ty=ty1}, {exp=exp2, ty=ty2}, pos) = (
                        if T.isSubtype (ty1, ty2) orelse T.isSubtype (ty2, ty1)
                        then case (ty1, ty2)
                            of  (T.NIL, T.NIL) => error pos "compare two nil's"
                            |   _ => ()
                        else error pos "compare values of different types";
                        (exp1, exp2)
                    )

                    fun checkOrderOperands (expty1, expty2, pos) = (
                        case (#ty expty1, #ty expty2)
                            of  (T.INT, T.INT) => ()
                            |   (T.STRING, T.STRING) => ()
                            |   _ => error pos "take order of non-int/string type";
                        checkComparisonOperands (expty1, expty2, pos)
                    )

                    fun checkUnit ({exp, ty}, pos) =
                        case ty
                            of  T.UNIT => exp
                            |   T.BOTTOM => exp
                            |   _ => (error pos "unit required"; ())

                    fun trexp (A.VarExp var) = trvar var
                    |   trexp A.NilExp = {exp=(), ty=T.NIL}
                    |   trexp (A.IntExp i) = {exp=(), ty=T.INT}
                    |   trexp (A.StringExp (s, pos)) = {exp=(), ty=T.STRING}
                    |   trexp (A.CallExp {func, args, pos}) = {exp=(), ty=T.BOTTOM} (* TODO *)
                    |   trexp (A.OpExp {left, oper=A.PlusOp, right, pos}) =
                        let val (exp1, exp2) = checkArithmeticOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.MinusOp, right, pos}) =
                        let val (exp1, exp2) = checkArithmeticOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.TimesOp, right, pos}) =
                        let val (exp1, exp2) = checkArithmeticOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.DivideOp, right, pos}) =
                        let val (exp1, exp2) = checkArithmeticOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.EqOp, right, pos}) =
                        let val (exp1, exp2) = checkComparisonOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.NeqOp, right, pos}) =
                        let val (exp1, exp2) = checkComparisonOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.LtOp, right, pos}) =
                        let val (exp1, exp2) = checkOrderOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.LeOp, right, pos}) =
                        let val (exp1, exp2) = checkOrderOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.GtOp, right, pos}) =
                        let val (exp1, exp2) = checkOrderOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
                    |   trexp (A.OpExp {left, oper=A.GeOp, right, pos}) =
                        let val (exp1, exp2) = checkOrderOperands (trexp left, trexp right, pos)
                        in {exp=(), ty=T.INT}
                        end
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
                    |   trexp (A.AssignExp {var, exp, pos}) = 
                        let val {exp=varExp, ty=varTy} = trvar var
                            val {exp=valExp, ty=valTy} = trexp exp
                        in
                            if T.isSubtype (valTy, varTy)
                            then ()
                            else error pos "assign value of wrong type";
                            {exp=(), ty=T.UNIT}
                        end
                    |   trexp (A.IfExp {test, then', else'=SOME else'', pos}) =
                        let val testExp = checkInt (trexp test, pos)
                            val {exp=thenExp, ty=thenTy} = trexp then'
                            val {exp=elseExp, ty=elseTy} = trexp else''
                        in
                            if T.isSubtype (thenTy, elseTy)
                            then {exp=(), ty=elseTy}
                            else if T.isSubtype (elseTy, thenTy)
                                then {exp=(), ty=thenTy}
                                else (
                                    error pos "types of if branches do not agree";
                                    {exp=(), ty=T.BOTTOM}
                                )
                        end
                    |   trexp (A.IfExp {test, then', else'=NONE, pos}) =
                        let val testExp = checkInt (trexp test, pos)
                            val thenExp = checkUnit (trexp then', pos)
                        in {exp=(), ty=T.UNIT}
                        end
                    |   trexp (A.WhileExp {test, body, pos}) =
                        let val testExp = checkInt (trexp test, pos)
                            val bodyExp = checkUnit (transExp (venv, tenv, true, body), pos)
                        in {exp=(), ty=T.UNIT}
                        end
                    |   trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
                        let val loExp = checkInt (trexp lo, pos)
                            val hiExp = checkInt (trexp hi, pos)
                            val venv' = S.enter (venv, var, E.VarEntry {ty=T.INT, forIdx=true})
                            val bodyExp = checkUnit (transExp (venv', tenv, true, body), pos)
                        in {exp=(), ty=T.UNIT}
                        end
                    |   trexp (A.BreakExp pos) =
                        if inLoop
                        then {exp=(), ty=T.BOTTOM}
                        else (error pos "break outside of loop"; {exp=(), ty=T.BOTTOM})
                    |   trexp (A.LetExp {decs, body, pos}) =
                        let val {venv=venv', tenv=tenv'} = transDecs (venv, tenv, decs)
                        in transExp (venv', tenv', inLoop, body)
                        end
                    |   trexp (A.ArrayExp {typ, size, init, pos}) =
                        let val (eleTy, unique) = case S.look (tenv, typ)
                                of  SOME ty => (
                                    case ty
                                        of  T.ARRAY (ty', unique) => (ty' (), unique)
                                        |   T.BOTTOM => (T.BOTTOM, ref())
                                        |   _ => (
                                            error pos ("create array with non-array type " ^ S.name typ);
                                            (T.BOTTOM, ref ())
                                        )
                                )
                                |   NONE => (unboundTypeError (typ, pos); (T.BOTTOM, ref ()))
                            val sizeExp = checkInt (trexp size, pos)
                            val {exp=initExp, ty=initTy} = trexp init
                        in
                            if T.isSubtype (initTy, eleTy) orelse T.isBottom eleTy
                            then ()
                            else error pos "inconsistent initial value type";
                            {exp=(), ty=T.ARRAY (fn () => eleTy, unique)}
                        end

                    and trvar (A.SimpleVar (id, pos)) = (
                        case S.look (venv, id)
                            of  SOME (E.VarEntry {ty, forIdx}) => {exp=(), ty=ty}
                            |   NONE => (
                                error pos ("unbound variable " ^ S.name id);
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
                        let val {exp=lvExp, ty=lvTy} = trvar lvalue
                            val idxExp = checkInt (trexp exp, pos)
                        in
                            case lvTy
                                of  T.ARRAY (eleTy, unique) => {exp=(), ty=eleTy ()}
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
            |   transDec (venv, tenv, A.VarDec {name, escape, typ=SOME (typ', typPos), init, pos}) =
                let val varTy = case S.look (tenv, typ')
                        of  SOME ty => ty
                        |   NONE => (unboundTypeError (typ', typPos); T.BOTTOM)
                    val {exp=valExp, ty=valTy} = transExp (venv, tenv, false, init)
                in 
                    if T.isSubtype (valTy, varTy) orelse T.isBottom varTy
                    then ()
                    else error pos "declare variable with wrong initial value type";
                    {venv=S.enter (venv, name, E.VarEntry {ty=varTy, forIdx=false}), tenv=tenv}
                end
            |   transDec (venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
                let val {exp=valExp, ty=valTy} = transExp (venv, tenv, false, init)
                    val varTy = case valTy
                        of  T.NIL => (error pos "nil value in un-typed variable declaration"; T.BOTTOM)
                        |   ty => ty
                in {venv=S.enter (venv, name, E.VarEntry {ty=varTy, forIdx=false}), tenv=tenv}
                end
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
                                            |   NONE => (unboundTypeError (name, pos);
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
                        in fn () => T.RECORD (foldr buildFields [] fieldList, unique)
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

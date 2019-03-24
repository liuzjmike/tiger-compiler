functor Semant (Translate : TRANSLATE) :
    sig val transProg : Absyn.exp -> unit end =
struct

    structure A = Absyn
    structure E = Env (Translate)
    structure S = Symbol
    structure T = Types
    val error = ErrorMsg.error

    fun unboundTypeError (ty, pos) = error pos ("unbound type " ^ S.name ty)

    fun canAccept (formal, actual) = T.isSubtype (actual, formal) orelse T.isBottom formal

    fun transTy (tenv, typ, pos) =
        case S.look (tenv, typ)
        of  SOME ty => ty
        |   NONE => (unboundTypeError (typ, pos); T.BOTTOM)

    fun transExp (venv, tenv, inLoop, exp, level) =
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

            fun checkOrderOperands (expty1, expty2, pos) =
                let fun hasOrder ty = case ty
                    of  T.INT => true
                    |   T.STRING => true
                    |   T.BOTTOM => true
                    |   _ => false
                in
                    if hasOrder (#ty expty1) andalso hasOrder (#ty expty2)
                    then ()
                    else error pos "take order of non-int/string type";
                checkComparisonOperands (expty1, expty2, pos)
                end

            fun checkUnit ({exp, ty}, pos) =
                case ty
                of  T.UNIT => exp
                |   T.BOTTOM => exp
                |   _ => (error pos "unit required"; ())

            fun trexp (A.VarExp var) = #1 (trvar var)
            |   trexp A.NilExp = {exp=(), ty=T.NIL}
            |   trexp (A.IntExp i) = {exp=(), ty=T.INT}
            |   trexp (A.StringExp (s, pos)) = {exp=(), ty=T.STRING}
            |   trexp (A.CallExp {func, args, pos}) = (
                case S.look (venv, func)
                of  SOME (E.FunEntry {level, label, formals, result}) =>
                    let val actuals = map trexp args
                        fun checkArgs ([], []) = ()
                        |   checkArgs ([], a2::l2) = error pos "too many arguments"
                        |   checkArgs (a1::l1, []) = error pos "insufficient arguments"
                        |   checkArgs (t1::l1, {exp,ty=t2}::l2) = (
                            if canAccept (t1, t2) then ()
                            else error pos "incorrect argument type";
                            checkArgs (l1, l2)
                        )
                    in
                        checkArgs (formals, actuals);
                        {exp=(), ty=result}
                    end
                |   SOME (E.VarEntry _) => (
                    error pos (S.name func ^ " is not a function");
                    {exp=(), ty=T.BOTTOM}
                )
                |   NONE => (
                    error pos ("unbound function " ^ S.name func);
                    {exp=(), ty=T.BOTTOM}
                )
            )
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
            |   trexp (A.RecordExp {fields, typ, pos}) = (
                case S.look (tenv, typ)
                of SOME (T.RECORD (fieldList, unique)) =>
                    let val fields = map (fn (id, exp, pos) => (id, trexp exp, pos)) fields
                        fun checkFields ([], []) = ()
                        |   checkFields ([], f2::l2) = error pos "too many fields in record creation"
                        |   checkFields (f1::l1, []) = error pos "insufficient fields in record creation"
                        |   checkFields ((id1, ty1)::l1, (id2, {exp, ty=ty2}, fieldPos)::l2) = (
                            if id1 = id2 then () else error fieldPos "incorrect field name";
                            if canAccept (ty1 (), ty2) then () else error fieldPos "incorrect field type";
                            checkFields (l1, l2)
                        )
                    in
                        checkFields (fieldList, fields);
                        {exp=(), ty=T.RECORD (fieldList, unique)}
                    end
                |   SOME T.BOTTOM => {exp=(), ty=T.BOTTOM}
                |   SOME _ => (
                    error pos "create record with non-record type";
                    {exp=(), ty=T.BOTTOM}
                )
                |   NONE => (unboundTypeError (typ, pos); {exp=(), ty=T.BOTTOM})
            )
            |   trexp (A.SeqExp expList) =
                let fun f [] = ([], T.UNIT)
                    |   f [(exp, pos)] = let val {exp=res, ty=ty} = trexp exp in ([res], ty) end
                    |   f ((exp, pos)::l) =
                        let val (resList, ty) = f l
                        in ((#exp (trexp exp))::resList, ty)
                        end
                    val (resList, ty) = f expList
                in {exp=(), ty=ty}
                end
            |   trexp (A.AssignExp {var, exp, pos}) =
                let val ({exp=varExp, ty=varTy}, forIdx) = trvar var
                    val {exp=valExp, ty=valTy} = trexp exp
                in
                    if forIdx
                    then error pos "cannot assign to for-loop index"
                    else ();
                    if canAccept (varTy, valTy) then ()
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
                    val bodyExp = checkUnit (transExp (venv, tenv, true, body, level), pos)
                in {exp=(), ty=T.UNIT}
                end
            |   trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
                let val loExp = checkInt (trexp lo, pos)
                    val hiExp = checkInt (trexp hi, pos)
                    val access = Translate.allocLocal level (!escape)
                    val venv' = S.enter (venv, var, E.VarEntry {
                        access=access, ty=T.INT, forIdx=true
                    })
                    val bodyExp = checkUnit (transExp (venv', tenv, true, body, level), pos)
                in {exp=(), ty=T.UNIT}
                end
            |   trexp (A.BreakExp pos) =
                if inLoop
                then {exp=(), ty=T.BOTTOM}
                else (error pos "break outside of loop"; {exp=(), ty=T.BOTTOM})
            |   trexp (A.LetExp {decs, body, pos}) =
                let val {venv=venv', tenv=tenv'} = transDecs (venv, tenv, decs, level)
                in transExp (venv', tenv', inLoop, body, level)
                end
            |   trexp (A.ArrayExp {typ, size, init, pos}) =
                let val (eleTy, unique) = case S.look (tenv, typ)
                    of  SOME ty => (
                        case ty
                        of  T.ARRAY (eleTy, unique) => (eleTy (), unique)
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
                    if canAccept (eleTy, initTy)
                    then ()
                    else error pos "incorrect array initial value type";
                    {exp=(), ty=T.ARRAY (fn () => eleTy, unique)}
                end

            and trvar (A.SimpleVar (id, pos)) = (
                case S.look (venv, id)
                of  SOME (E.VarEntry {access, ty, forIdx}) => ({exp=(), ty=ty}, forIdx)
                |   SOME (E.FunEntry _) => (
                    error pos (S.name id ^ "is a function, not a variable");
                    ({exp=(), ty=T.BOTTOM}, false)
                )
                |   NONE => (
                    error pos ("unbound variable " ^ S.name id);
                    ({exp=(), ty=T.BOTTOM}, false)
                )
            )
            |   trvar (A.FieldVar (lvalue, id, pos)) =
                let val {exp=exp, ty=ty} = #1 (trvar lvalue)
                    fun f ([], i) = (error pos ("field " ^ S.name id ^ " does not exist"); ({exp=(), ty=T.BOTTOM}, false))
                    |   f ((id', ty')::l, i) = if id' = id then ({exp=(), ty=ty'()}, false) else f (l, i+1)
                in case ty
                    of  T.RECORD (fieldList, unique) => f (fieldList, 0)
                    |   T.BOTTOM => ({exp=(), ty=T.BOTTOM}, false)
                    |   _ => (error pos "access field of non-record type"; ({exp=(), ty=T.BOTTOM}, false))
                end
            |   trvar (A.SubscriptVar (lvalue, idx, pos)) =
                let val {exp=lvExp, ty=lvTy} = #1 (trvar lvalue)
                    val idxExp = checkInt (trexp idx, pos)
                in case lvTy
                    of  T.ARRAY (eleTy, unique) => ({exp=(), ty=eleTy ()}, false)
                    |   T.BOTTOM => ({exp=(), ty=T.BOTTOM}, false)
                    |   _ => (error pos "index non-array type"; ({exp=(), ty=T.BOTTOM}, false))
                end
        in
            trexp exp
        end

    and transDecs (venv, tenv, decs, level) =
        let fun transDec (venv, tenv, A.FunctionDec decList) =
                let fun transparam {name, typ, escape, pos} = {
                        name=name,
                        ty=transTy (tenv, typ, pos),
                        escape=(!escape)
                    }
                    fun addFunToEnv ({name, params, result, body, pos}, (venv, localEnv, decList)) =
                        let val resultTy = case result
                                of  SOME (typ, pos) => transTy (tenv, typ, pos)
                                |   NONE => T.UNIT
                            val params' = map transparam params
                            val localEnv = case S.look (localEnv, name)
                                of  SOME () => (
                                    error pos  (
                                        "functions of same name "
                                        ^ S.name name
                                        ^ " in one mutually recursive group"
                                    );
                                    localEnv
                                    )
                                |   NONE => S.enter (localEnv, name, ())
                            val label = Temp.newlabel ()
                            val funLevel = Translate.newLevel {
                                parent=level,
                                name=label,
                                formals=map (fn x => !(#escape x)) params
                            }
                        in
                            (
                                S.enter (
                                    venv, name,
                                    E.FunEntry {
                                        level=funLevel, label=label,
                                        formals=map #ty params', result=resultTy
                                    }),
                                localEnv,
                                (name, params', resultTy, body, pos, funLevel)::decList
                            )
                        end
                    val (venv', localEnv, decList') = foldl addFunToEnv (venv, S.empty, []) decList
                    val decList' = List.rev decList'
                    fun checkBody (name, params, result, body, pos, funLevel) =
                        let fun enterparam ({name, ty, escape}, venv) = S.enter (
                                venv, name,
                                E.VarEntry {
                                    access=Translate.allocLocal funLevel escape,
                                    ty=ty, forIdx=false
                            })
                            val venv'' = foldl enterparam venv' params
                            val {exp=bodyExp, ty=bodyTy} = transExp (venv'', tenv, false, body, funLevel)
                        in
                            if canAccept (result, bodyTy)
                            then ()
                            else error pos "incorrect return type"
                        end
                in
                    map checkBody decList';
                    {venv=venv', tenv=tenv}
                end
            |   transDec (venv, tenv, A.VarDec {name, escape, typ=SOME (typ, typPos), init, pos}) =
                let val varTy = transTy (tenv, typ, typPos)
                    val access = Translate.allocLocal level (!escape)
                    val {exp=valExp, ty=valTy} = transExp (venv, tenv, false, init, level)
                in
                    if canAccept (varTy, valTy)
                    then ()
                    else error pos "declare variable with wrong initial value type";
                    {
                        venv=S.enter (
                            venv, name,
                            E.VarEntry {
                                access=access,
                                ty=varTy, forIdx=false
                            }),
                        tenv=tenv
                    }
                end
            |   transDec (venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
                let val access = Translate.allocLocal level (!escape)
                    val {exp=valExp, ty=valTy} = transExp (venv, tenv, false, init, level)
                    val varTy = case valTy
                        of  T.NIL => (error pos "nil value in variable declaration not contrained by record type"; T.BOTTOM)
                        |   ty => ty
                in {
                    venv=S.enter (
                        venv, name,
                        E.VarEntry {
                            access=access,
                            ty=varTy, forIdx=false
                        }
                    ),
                    tenv=tenv
                }
                end
            |   transDec (venv, tenv, A.TypeDec decList) =
                let val decList = map (fn dec => (dec, ref ())) decList
                    fun addToEnv (transform, check) (({name, ty, pos}, unique), t) = (
                        check (t, name, pos);
                        S.enter (t, name, transform (name, ty, unique))
                    )
                    val localTEnv = foldl (addToEnv (
                            fn (name, ty, unique) => (ty, unique),
                            fn (t, name, pos) => case S.look (t, name)
                                of  SOME _ => error pos (
                                        "types of same name "
                                        ^ S.name name
                                        ^ " in one mutually recursive group"
                                    )
                                |   NONE => ()
                        )) S.empty decList
                    fun getType (name, pos, seen) =
                        case S.look(localTEnv, name)
                        of  SOME (ty, unique) => transTyDec (name, ty, unique, seen)
                        |   NONE => case S.look(tenv, name)
                                    of  SOME ty => (fn () => ty)
                                    |   NONE => (
                                        unboundTypeError (name, pos);
                                        fn () => T.BOTTOM
                                    )
                    and transTyDec (name, A.NameTy (typ, pos), unique, seen) =
                        let val seen = S.enter (seen, name, ())
                        in case S.look (seen, typ)
                            of  SOME _ => (error pos "circular type aliasing"; fn () => T.BOTTOM)
                            |   NONE => fn () => getType (typ, pos, seen) ()
                        end
                    |   transTyDec (name, A.RecordTy (fieldList), unique, seen
                        ) =
                        let fun buildFields ({name, escape, typ, pos}, l) = (name, getType (typ, pos, seen))::l
                        in fn () => T.RECORD (foldr buildFields [] fieldList, unique)
                        end
                    |   transTyDec (name, A.ArrayTy (typ, pos), unique, seen) =
                            (fn () => T.ARRAY (getType (typ, pos, seen), unique))
                in
                    {
                        venv=venv,
                        tenv=foldl (addToEnv (
                            fn (name, ty, unique) => transTyDec (name, ty, unique, S.empty) (),
                            fn (t, name, pos) => ()
                        )) tenv decList
                    }
                end
            fun buildEnv (dec, {venv, tenv}) = transDec (venv, tenv, dec)
        in foldl buildEnv {venv=venv, tenv=tenv} decs
        end

    fun transProg exp =
        let val level = Translate.newLevel {parent=Translate.outermost, name=Temp.namedlabel "tig-main", formals=[]}
        in
            FindEscape.findEscape exp;
            #exp (transExp (E.base_venv, E.base_tenv, false, exp, level))
        end
end

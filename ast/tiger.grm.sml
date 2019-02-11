functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
type symbol = Symbol.symbol
val symbol = Symbol.symbol

fun makeVar (id, pos) = A.SimpleVar(symbol id, pos)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\149\000\005\000\149\000\007\000\149\000\009\000\149\000\
\\011\000\149\000\013\000\149\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\149\000\027\000\149\000\
\\031\000\149\000\032\000\149\000\035\000\149\000\036\000\149\000\
\\038\000\149\000\039\000\149\000\043\000\149\000\044\000\149\000\
\\045\000\149\000\000\000\
\\001\000\001\000\150\000\005\000\150\000\007\000\150\000\009\000\150\000\
\\011\000\150\000\013\000\150\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\150\000\027\000\150\000\
\\031\000\150\000\032\000\150\000\035\000\150\000\036\000\150\000\
\\038\000\150\000\039\000\150\000\043\000\150\000\044\000\150\000\
\\045\000\150\000\000\000\
\\001\000\001\000\151\000\005\000\151\000\007\000\151\000\009\000\151\000\
\\011\000\151\000\013\000\151\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\151\000\027\000\151\000\
\\031\000\151\000\032\000\151\000\035\000\151\000\036\000\151\000\
\\038\000\151\000\039\000\151\000\043\000\151\000\044\000\151\000\
\\045\000\151\000\000\000\
\\001\000\001\000\152\000\005\000\152\000\007\000\152\000\009\000\152\000\
\\011\000\152\000\013\000\152\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\152\000\027\000\152\000\
\\031\000\152\000\032\000\152\000\035\000\152\000\036\000\152\000\
\\038\000\152\000\039\000\152\000\043\000\152\000\044\000\152\000\
\\045\000\152\000\000\000\
\\001\000\001\000\153\000\005\000\153\000\007\000\153\000\009\000\153\000\
\\011\000\153\000\013\000\153\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\153\000\027\000\153\000\
\\031\000\153\000\032\000\153\000\035\000\153\000\036\000\153\000\
\\038\000\153\000\039\000\153\000\043\000\153\000\044\000\153\000\
\\045\000\153\000\000\000\
\\001\000\001\000\154\000\005\000\154\000\007\000\154\000\009\000\154\000\
\\011\000\154\000\013\000\154\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\154\000\027\000\154\000\
\\031\000\154\000\032\000\154\000\035\000\154\000\036\000\154\000\
\\038\000\154\000\039\000\154\000\043\000\154\000\044\000\154\000\
\\045\000\154\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\037\000\000\000\
\\001\000\002\000\065\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\002\000\075\000\000\000\
\\001\000\002\000\079\000\000\000\
\\001\000\002\000\104\000\012\000\103\000\029\000\102\000\000\000\
\\001\000\002\000\106\000\000\000\
\\001\000\002\000\110\000\000\000\
\\001\000\002\000\127\000\000\000\
\\001\000\002\000\131\000\000\000\
\\001\000\006\000\123\000\000\000\
\\001\000\008\000\088\000\000\000\
\\001\000\009\000\071\000\000\000\
\\001\000\009\000\098\000\000\000\
\\001\000\009\000\122\000\000\000\
\\001\000\011\000\097\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\001\000\011\000\113\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\001\000\013\000\095\000\000\000\
\\001\000\013\000\128\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\031\000\070\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\035\000\111\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\036\000\069\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\036\000\132\000\000\000\
\\001\000\020\000\085\000\000\000\
\\001\000\020\000\096\000\000\000\
\\001\000\020\000\133\000\000\000\
\\001\000\028\000\068\000\000\000\
\\001\000\028\000\105\000\000\000\
\\001\000\038\000\064\000\000\000\
\\001\000\039\000\100\000\000\000\
\\001\000\040\000\118\000\000\000\
\\137\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\138\000\028\000\016\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\018\000\026\000\019\000\025\000\000\000\
\\146\000\018\000\026\000\019\000\025\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\155\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\000\000\
\\156\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\000\000\
\\157\000\000\000\
\\158\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\159\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\160\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\161\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\032\000\112\000\000\000\
\\162\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\163\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\008\000\049\000\010\000\048\000\012\000\047\000\014\000\046\000\000\000\
\\167\000\010\000\074\000\014\000\046\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\171\000\040\000\116\000\000\000\
\\172\000\000\000\
\\173\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\174\000\000\000\
\\175\000\007\000\072\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\176\000\000\000\
\\177\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\178\000\000\000\
\\179\000\005\000\099\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\180\000\000\000\
\\181\000\002\000\079\000\000\000\
\\182\000\000\000\
\\183\000\005\000\094\000\000\000\
\\184\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\185\000\000\000\
\\186\000\043\000\036\000\044\000\035\000\045\000\034\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\191\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\192\000\000\000\
\\193\000\006\000\087\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\002\000\110\000\000\000\
\\200\000\000\000\
\\201\000\005\000\121\000\000\000\
\\202\000\000\000\
\"
val actionRowNumbers =
"\007\000\041\000\040\000\042\000\
\\062\000\084\000\008\000\007\000\
\\007\000\007\000\071\000\045\000\
\\044\000\063\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\087\000\
\\086\000\085\000\084\000\037\000\
\\009\000\010\000\011\000\035\000\
\\030\000\028\000\046\000\070\000\
\\021\000\073\000\066\000\064\000\
\\012\000\079\000\007\000\075\000\
\\056\000\053\000\052\000\006\000\
\\005\000\004\000\003\000\002\000\
\\001\000\051\000\050\000\049\000\
\\048\000\083\000\071\000\032\000\
\\091\000\020\000\007\000\007\000\
\\007\000\043\000\007\000\065\000\
\\007\000\067\000\081\000\078\000\
\\026\000\033\000\024\000\074\000\
\\022\000\077\000\038\000\014\000\
\\036\000\015\000\097\000\029\000\
\\059\000\058\000\072\000\025\000\
\\013\000\054\000\007\000\069\000\
\\047\000\007\000\061\000\092\000\
\\039\000\097\000\093\000\007\000\
\\090\000\099\000\096\000\023\000\
\\019\000\007\000\007\000\068\000\
\\080\000\082\000\007\000\076\000\
\\017\000\027\000\088\000\016\000\
\\091\000\018\000\031\000\057\000\
\\055\000\095\000\094\000\098\000\
\\034\000\100\000\007\000\007\000\
\\060\000\089\000\000\000"
val gotoT =
"\
\\001\000\134\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\031\000\014\000\030\000\015\000\029\000\016\000\028\000\
\\018\000\027\000\000\000\
\\000\000\
\\002\000\036\000\003\000\001\000\000\000\
\\002\000\037\000\003\000\001\000\000\000\
\\002\000\038\000\003\000\001\000\000\000\
\\002\000\041\000\003\000\001\000\006\000\040\000\007\000\039\000\000\000\
\\000\000\
\\000\000\
\\004\000\043\000\005\000\042\000\000\000\
\\002\000\048\000\003\000\001\000\000\000\
\\002\000\049\000\003\000\001\000\000\000\
\\002\000\050\000\003\000\001\000\000\000\
\\002\000\051\000\003\000\001\000\000\000\
\\002\000\052\000\003\000\001\000\000\000\
\\002\000\053\000\003\000\001\000\000\000\
\\002\000\054\000\003\000\001\000\000\000\
\\002\000\055\000\003\000\001\000\000\000\
\\002\000\056\000\003\000\001\000\000\000\
\\002\000\057\000\003\000\001\000\000\000\
\\002\000\058\000\003\000\001\000\000\000\
\\002\000\059\000\003\000\001\000\000\000\
\\002\000\060\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\061\000\014\000\030\000\015\000\029\000\016\000\028\000\
\\018\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\071\000\000\000\
\\000\000\
\\010\000\076\000\011\000\075\000\012\000\074\000\000\000\
\\002\000\078\000\003\000\001\000\000\000\
\\002\000\081\000\003\000\001\000\008\000\080\000\009\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\041\000\003\000\001\000\006\000\082\000\007\000\039\000\000\000\
\\000\000\
\\017\000\084\000\000\000\
\\000\000\
\\002\000\087\000\003\000\001\000\000\000\
\\002\000\088\000\003\000\001\000\000\000\
\\002\000\089\000\003\000\001\000\000\000\
\\000\000\
\\002\000\041\000\003\000\001\000\007\000\090\000\000\000\
\\000\000\
\\002\000\091\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\099\000\000\000\
\\000\000\
\\000\000\
\\020\000\107\000\021\000\106\000\022\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\112\000\012\000\074\000\000\000\
\\000\000\
\\002\000\113\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\081\000\003\000\001\000\009\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\117\000\021\000\106\000\022\000\105\000\000\000\
\\000\000\
\\002\000\118\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\122\000\003\000\001\000\000\000\
\\002\000\123\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\124\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\127\000\022\000\105\000\000\000\
\\017\000\128\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\132\000\003\000\001\000\000\000\
\\002\000\133\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 135
val numrules = 66
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | tyfield of unit ->  (A.field)
 | tyfieldlist of unit ->  (A.field list)
 | tyfieldlist_opt of unit ->  (A.field list) | ty of unit ->  (A.ty)
 | tydec of unit ->  (A.dec)
 | tyarg_opt of unit ->  ( ( symbol * pos )  option)
 | fundec of unit ->  (A.dec) | vardec of unit ->  (A.dec)
 | dec of unit ->  (A.dec) | decs of unit ->  (A.dec list)
 | field of unit ->  (symbol*A.exp*pos)
 | fieldlist of unit ->  ( ( symbol * A.exp * pos )  list)
 | fieldlist_opt of unit ->  ( ( symbol * A.exp * pos )  list)
 | explist of unit ->  (A.exp list)
 | explist_opt of unit ->  (A.exp list)
 | expseq of unit ->  ( ( A.exp * pos )  list)
 | expseq_opt of unit ->  (A.exp)
 | access of unit ->  (A.var -> A.var)
 | accseq of unit ->  (A.var -> A.var) | lvalue of unit ->  (A.var)
 | exp of unit ->  (A.exp) | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp lvalue)
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expseq_opt 
expseq_opt1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (expseq_opt as 
expseq_opt1) = expseq_opt1 ()
 in (expseq_opt)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp INT)
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp{left=A.IntExp 0, oper=A.MinusOp, right=exp, pos=MINUSleft})

end)
 in ( LrTable.NT 1, ( result, MINUS1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.explist_opt 
explist_opt1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (ID as ID1) = ID1 ()
 val  (explist_opt as explist_opt1) = explist_opt1 ()
 in (A.CallExp{func=symbol ID, args=explist_opt, pos=IDleft})
end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.PlusOp, right=exp2, pos=PLUSleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.MinusOp, right=exp2, pos=MINUSleft})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.TimesOp, right=exp2, pos=TIMESleft})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.DivideOp, right=exp2, pos=DIVIDEleft})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
EQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.EqOp, right=exp2, pos=EQleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.NeqOp, right=exp2, pos=NEQleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.LtOp, right=exp2, pos=LTleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.LeOp, right=exp2, pos=LEleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.GtOp, right=exp2, pos=GTleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.GeOp, right=exp2, pos=GEleft})
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ANDleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test=exp1, then'=exp2, else'=SOME (A.IntExp 0), pos=ANDleft})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
ORleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp{test=exp1, then'=A.IntExp 1, else'=SOME exp2, pos=ORleft}
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.fieldlist_opt fieldlist_opt1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (fieldlist_opt as fieldlist_opt1) = fieldlist_opt1 ()
 in (A.RecordExp{fields=fieldlist_opt, typ=symbol ID, pos=IDleft})
end
)
 in ( LrTable.NT 1, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.ArrayExp{typ=symbol ID, size=exp1, init=exp2, pos=IDleft})
end)
 in ( LrTable.NT 1, ( result, ID1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) ::
 rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lvalue
 as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp{var=lvalue, exp=exp, pos=ASSIGNleft})
end)
 in ( LrTable.NT 1, ( result, lvalue1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.IfExp{test=exp1, then'=exp2, else'=SOME exp3, pos=IFleft})
end)
 in ( LrTable.NT 1, ( result, IF1left, exp3right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp{test=exp1, then'=exp2, else'=NONE, pos=IFleft})
end)
 in ( LrTable.NT 1, ( result, IF1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp{test=exp1, body=exp2, pos=WHILEleft})
end)
 in ( LrTable.NT 1, ( result, WHILE1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp{var=symbol ID, escape=ref true, lo=exp1, hi=exp2, body=exp3, pos=FORleft}
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, exp3right), rest671)
end
|  ( 27, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expseq_opt 
expseq_opt1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _,
 ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (expseq_opt as expseq_opt1) = expseq_opt1 ()
 in (A.LetExp{decs=decs, body=expseq_opt, pos=LETleft})
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 28, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp BREAKleft))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (makeVar(ID, IDleft))
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.accseq accseq1, _, accseq1right)) :: ( _, (
 MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1 ()
 val  (accseq as accseq1) = accseq1 ()
 in (accseq(makeVar(ID, IDleft)))
end)
 in ( LrTable.NT 2, ( result, ID1left, accseq1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.access access1, _, access1right)) :: ( _, (
 MlyValue.accseq accseq1, accseq1left, _)) :: rest671)) => let val  
result = MlyValue.accseq (fn _ => let val  (accseq as accseq1) = 
accseq1 ()
 val  (access as access1) = access1 ()
 in (fn var => access(accseq(var)))
end)
 in ( LrTable.NT 3, ( result, accseq1left, access1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.access access1, access1left, access1right))
 :: rest671)) => let val  result = MlyValue.accseq (fn _ => let val  (
access as access1) = access1 ()
 in (access)
end)
 in ( LrTable.NT 3, ( result, access1left, access1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, (DOTleft
 as DOT1left), _)) :: rest671)) => let val  result = MlyValue.access
 (fn _ => let val  (ID as ID1) = ID1 ()
 in (fn var => A.FieldVar(var, symbol ID, DOTleft))
end)
 in ( LrTable.NT 4, ( result, DOT1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, (LBRACKleft as LBRACK1left), _)) :: rest671)) => let
 val  result = MlyValue.access (fn _ => let val  (exp as exp1) = exp1
 ()
 in (fn var => A.SubscriptVar(var, exp, LBRACKleft))
end)
 in ( LrTable.NT 4, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.expseq expseq1, expseq1left, expseq1right))
 :: rest671)) => let val  result = MlyValue.expseq_opt (fn _ => let
 val  (expseq as expseq1) = expseq1 ()
 in (A.SeqExp expseq)
end)
 in ( LrTable.NT 5, ( result, expseq1left, expseq1right), rest671)
end
|  ( 36, ( rest671)) => let val  result = MlyValue.expseq_opt (fn _ =>
 (A.SeqExp nil))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.expseq expseq1, _, expseq1right)) :: _ :: (
 _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) =>
 let val  result = MlyValue.expseq (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (expseq as expseq1) = expseq1 ()
 in ((exp, expleft)::expseq)
end)
 in ( LrTable.NT 6, ( result, exp1left, expseq1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.expseq (fn _ => let val 
 (exp as exp1) = exp1 ()
 in ((exp, expleft)::nil)
end)
 in ( LrTable.NT 6, ( result, exp1left, exp1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.explist explist1, explist1left, 
explist1right)) :: rest671)) => let val  result = MlyValue.explist_opt
 (fn _ => let val  (explist as explist1) = explist1 ()
 in (explist)
end)
 in ( LrTable.NT 7, ( result, explist1left, explist1right), rest671)

end
|  ( 40, ( rest671)) => let val  result = MlyValue.explist_opt (fn _
 => (nil))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( MlyValue.explist explist1, _, explist1right)) :: _
 :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.explist (fn _ => let val  (exp as exp1) = exp1 ()
 val  (explist as explist1) = explist1 ()
 in (exp::explist)
end)
 in ( LrTable.NT 8, ( result, exp1left, explist1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.explist (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp::nil)
end)
 in ( LrTable.NT 8, ( result, exp1left, exp1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.fieldlist fieldlist1, fieldlist1left, 
fieldlist1right)) :: rest671)) => let val  result = 
MlyValue.fieldlist_opt (fn _ => let val  (fieldlist as fieldlist1) = 
fieldlist1 ()
 in (fieldlist)
end)
 in ( LrTable.NT 9, ( result, fieldlist1left, fieldlist1right), 
rest671)
end
|  ( 44, ( rest671)) => let val  result = MlyValue.fieldlist_opt (fn _
 => (nil))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 45, ( ( _, ( MlyValue.fieldlist fieldlist1, _, fieldlist1right))
 :: _ :: ( _, ( MlyValue.field field1, field1left, _)) :: rest671)) =>
 let val  result = MlyValue.fieldlist (fn _ => let val  (field as 
field1) = field1 ()
 val  (fieldlist as fieldlist1) = fieldlist1 ()
 in (field::fieldlist)
end)
 in ( LrTable.NT 10, ( result, field1left, fieldlist1right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.field field1, field1left, field1right)) :: 
rest671)) => let val  result = MlyValue.fieldlist (fn _ => let val  (
field as field1) = field1 ()
 in (field::nil)
end)
 in ( LrTable.NT 10, ( result, field1left, field1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.field (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((symbol ID, exp, IDleft))
end)
 in ( LrTable.NT 11, ( result, ID1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in (

                                            case (dec, decs) of
                                                (A.FunctionDec [f1], (A.FunctionDec f2)::l) => (A.FunctionDec (f1::f2))::l
                                              | (A.TypeDec [t1], (A.TypeDec t2)::l) => (A.TypeDec (t1::t2))::l
                                              | (dec, decs) => dec::decs
                                        
)
end)
 in ( LrTable.NT 12, ( result, dec1left, decs1right), rest671)
end
|  ( 49, ( rest671)) => let val  result = MlyValue.decs (fn _ => (nil)
)
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 50, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardec as vardec1) = vardec1 ()
 in (vardec)
end)
 in ( LrTable.NT 13, ( result, vardec1left, vardec1right), rest671)

end
|  ( 51, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
fundec as fundec1) = fundec1 ()
 in (fundec)
end)
 in ( LrTable.NT 13, ( result, fundec1left, fundec1right), rest671)

end
|  ( 52, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.dec (fn _ => let val  (tydec
 as tydec1) = tydec1 ()
 in (tydec)
end)
 in ( LrTable.NT 13, ( result, tydec1left, tydec1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.tyarg_opt tyarg_opt1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)
) :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (tyarg_opt as tyarg_opt1) = tyarg_opt1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec{name=symbol ID, escape=ref true,
                                         typ=tyarg_opt, init=exp, pos=VARleft}
)
end)
 in ( LrTable.NT 14, ( result, VAR1left, exp1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.tyarg_opt tyarg_opt1, _, _)) :: _ :: ( _, ( 
MlyValue.tyfieldlist_opt tyfieldlist_opt1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left),
 _)) :: rest671)) => let val  result = MlyValue.fundec (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (tyfieldlist_opt as tyfieldlist_opt1) = tyfieldlist_opt1 ()
 val  (tyarg_opt as tyarg_opt1) = tyarg_opt1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec
                                         [{name=symbol ID, params=tyfieldlist_opt,
                                           result=tyarg_opt, body=exp, pos=FUNCTIONleft}
                                         ]
)
end)
 in ( LrTable.NT 15, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ID ID1, IDleft, ID1right)) :: ( _, ( _, 
COLON1left, _)) :: rest671)) => let val  result = MlyValue.tyarg_opt
 (fn _ => let val  (ID as ID1) = ID1 ()
 in (SOME (symbol ID, IDleft))
end)
 in ( LrTable.NT 16, ( result, COLON1left, ID1right), rest671)
end
|  ( 56, ( rest671)) => let val  result = MlyValue.tyarg_opt (fn _ =>
 (NONE))
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (A.TypeDec [{name=symbol ID, ty=ty, pos=TYPEleft}])
end)
 in ( LrTable.NT 17, ( result, TYPE1left, ty1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(symbol ID, IDleft))
end)
 in ( LrTable.NT 18, ( result, ID1left, ID1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.tyfieldlist_opt tyfieldlist_opt1, _, _)) :: ( _, ( _, 
LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ty (fn _
 => let val  (tyfieldlist_opt as tyfieldlist_opt1) = tyfieldlist_opt1
 ()
 in (A.RecordTy tyfieldlist_opt)
end)
 in ( LrTable.NT 18, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(symbol ID, ARRAYleft))
end)
 in ( LrTable.NT 18, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.tyfieldlist tyfieldlist1, tyfieldlist1left,
 tyfieldlist1right)) :: rest671)) => let val  result = 
MlyValue.tyfieldlist_opt (fn _ => let val  (tyfieldlist as 
tyfieldlist1) = tyfieldlist1 ()
 in (tyfieldlist)
end)
 in ( LrTable.NT 19, ( result, tyfieldlist1left, tyfieldlist1right), 
rest671)
end
|  ( 62, ( rest671)) => let val  result = MlyValue.tyfieldlist_opt (fn
 _ => (nil))
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 63, ( ( _, ( MlyValue.tyfieldlist tyfieldlist1, _, 
tyfieldlist1right)) :: _ :: ( _, ( MlyValue.tyfield tyfield1, 
tyfield1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfieldlist (fn _ => let val  (tyfield as tyfield1) = 
tyfield1 ()
 val  (tyfieldlist as tyfieldlist1) = tyfieldlist1 ()
 in (tyfield::tyfieldlist)
end)
 in ( LrTable.NT 20, ( result, tyfield1left, tyfieldlist1right), 
rest671)
end
|  ( 64, ( ( _, ( MlyValue.tyfield tyfield1, tyfield1left, 
tyfield1right)) :: rest671)) => let val  result = MlyValue.tyfieldlist
 (fn _ => let val  (tyfield as tyfield1) = tyfield1 ()
 in (tyfield::nil)
end)
 in ( LrTable.NT 20, ( result, tyfield1left, tyfield1right), rest671)

end
|  ( 65, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfield (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ({name=symbol ID1, escape=ref true, typ=symbol ID2, pos=ID1left})

end)
 in ( LrTable.NT 21, ( result, ID1left, ID2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
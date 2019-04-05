functor IntMapTable (type key
		                 val getInt: key -> int) : TABLE =
struct
  type key = key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty

  fun enter(t,k,a) = IntBinaryMap.insert(t,getInt k,a)
  fun equal(t1,t2) =
    let fun containsAll [] = true
          | containsAll ((k,a)::l) =
            case IntBinaryMap.find(t1,k) of
                SOME a2 => if a=a2 then containsAll l else false
              | NONE => false
    in
      if IntBinaryMap.numItems t1 = IntBinaryMap.numItems t2
      then containsAll (IntBinaryMap.listItemsi t2)
      else false
    end
  fun items t = IntBinaryMap.listItems t
  fun look(t,k) = IntBinaryMap.find(t,getInt k)
  fun remove(t,k) =
    let val (t',a) = IntBinaryMap.remove(t,getInt k)
    in t'
    end
    handle NotFound => t
  fun remove'(k,t) = remove(t,k)
  fun update(t1,t2) = IntBinaryMap.foldli 
                     (fn (k,a,t) => IntBinaryMap.insert(t,k,a))
                     t1 t2
end

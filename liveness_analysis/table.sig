signature TABLE = 
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val equal : ''a table * ''a table -> bool
   val items : 'a table -> 'a list
   val look  : 'a table * key -> 'a option
   val remove : 'a table * key -> 'a table
   val remove' : key * 'a table -> 'a table
   val update : 'a table * 'a table -> 'a table
end

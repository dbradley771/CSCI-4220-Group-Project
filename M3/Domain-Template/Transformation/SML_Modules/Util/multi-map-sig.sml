signature MULTI_MAP =
sig
    structure Map : ORD_MAP
    
    type 'a map
    
    val empty : 'a map

    val insert : ('a map * Map.Key.ord_key * 'a) -> 'a map
    val insert' : ((Map.Key.ord_key * 'a) * 'a map) -> 'a map
    val find : ('a map * Map.Key.ord_key) -> 'a list
    val remove : ('a map * Map.Key.ord_key) -> ('a map * 'a list)

    val numItems : 'a map -> int

    val listItems : 'a map -> 'a list
    val listItemsi : 'a map -> (Map.Key.ord_key * 'a) list

(*
    val collate : (('a * 'a) -> order) -> ('a map * 'a map) -> order

    val unionWith : (('a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : ((Key.ord_key * 'a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map
    val intersectWith : (('a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : ((Key.ord_key * 'a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map
*)

    val app : ('a -> unit) -> 'a map -> unit
    val appi : ((Map.Key.ord_key * 'a) -> unit) -> 'a map -> unit
    val map : ('a -> 'b) -> 'a map -> 'b map
    val mapi : ((Map.Key.ord_key * 'a) -> 'b) -> 'a map -> 'b map

(*
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldli : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldri : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b
*)

    val filter : ('a -> bool) -> 'a map -> 'a map
    val filteri : ((Map.Key.ord_key * 'a) -> bool) -> 'a map -> 'a map
(*
    val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : ((Key.ord_key * 'a) -> 'b option) -> 'a map -> 'b map
*)
end
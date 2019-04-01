(* Abstract signature of a scoped symbol table. *)

signature SCOPED_MAP =
sig
    structure Map : ORD_MAP

    type 'a scoped_map

    val empty   : 'a scoped_map
    val insert  : 'a scoped_map * Map.Key.ord_key * 'a -> 'a scoped_map
    val insert' : (Map.Key.ord_key * 'a) * 'a scoped_map -> 'a scoped_map
    val find    : 'a scoped_map * Map.Key.ord_key -> 'a option

    val enter   : 'a scoped_map -> 'a scoped_map
    val exit    : 'a scoped_map -> 'a scoped_map

    val depth   : 'a scoped_map -> int
    val nth     : 'a scoped_map * int -> 'a Map.map
    val depthOf : 'a scoped_map * Map.Key.ord_key -> int option

    (* map applies to all scopes *)
    val map     : ('a -> 'b) -> 'a scoped_map -> 'b scoped_map
    val mapi    : ((Map.Key.ord_key * 'a) -> 'b) -> 'a scoped_map -> 'b scoped_map
end

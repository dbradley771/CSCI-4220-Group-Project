(* An implementation of a scoped symbol table. *)

functor ListMultiMapFn (structure M : ORD_MAP) : MULTI_MAP =
struct
    structure Map = M

    type 'a map = ('a list) M.map

    val empty = M.empty

    fun insert (map, key, value) =
        let
            val values =
                case M.find (map, key) of
                    SOME values => values
                  | NONE        => []
        in
            M.insert (map, key, value :: values)
        end

    fun insert' ((key, value), map) = insert (map, key, value)

    fun find (map, key) =
        (
            case M.find (map, key) of
               SOME values => values
             | NONE        => []
        )

    fun remove (map, key) = M.remove (map, key)

    fun numItems map =
        M.foldl
            (fn (values, acc) => List.length values + acc)
            0 map

    fun listItems map = M.foldr (List.@) [] map

    fun listItemsi map =
        M.foldri
            (fn (key, values, acc) =>
                (List.map
                    (fn value => (key, value))
                    values)
                @ acc)
            [] map

    fun app f map = M.app (List.app f) map

    fun appi f map =
        M.appi
            (fn (key, values) =>
                List.app
                    (fn value => f (key, value))
                    values)
            map

    fun map f map = M.map (List.map f) map

    fun mapi f map =
        M.mapi
            (fn (key, values) =>
                List.map
                    (fn value => f (key, value))
                    values)
            map

    fun filter f map =
        M.mapPartial
            (fn values =>
                let
                    val values = List.filter f values
                in
                    if List.null values then
                        NONE
                    else
                        SOME values
                end)
        map

    fun filteri f map =
        M.mapPartiali
            (fn (key, values) =>
                let
                    val values = List.filter (fn value => f (key, value)) values
                in
                    if List.null values then
                        NONE
                    else
                        SOME values
                end)
        map
end

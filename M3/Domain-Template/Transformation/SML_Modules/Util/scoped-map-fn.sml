(* An implementation of a scoped symbol table. *)

functor ScopedMapFn (structure M : ORD_MAP) : SCOPED_MAP =
struct
    structure Map = M

    type 'a scoped_map = int * ((int * 'a) list) M.map

    val empty = (0, M.empty)

    fun insert ((scope, map), key, value) =
        let
            val hiddenValues =
                case M.find (map, key) of
                    SOME values => values
                  | NONE        => []
        in
            (scope, M.insert (map, key, (scope, value) :: hiddenValues))
        end

    fun insert' ((key, value), map) = insert (map, key, value)

    fun find ((_, map), key) =
        (
            case M.find (map, key) of
               SOME ((_, value) :: _) => SOME value
             | NONE                   => NONE
             | SOME []                => raise LibBase.Impossible "key with empty values"
        )

    fun enter (scope, map) = (scope + 1, map)

    fun exit (scope, map) =
        let
            (* remove outermost scope values, remove empty value lists *)
            val map =
                M.mapPartial
                    (fn (values as ((valueScope, value) :: hiddenValues)) =>
                            if valueScope = scope then
                                if List.null hiddenValues then
                                    NONE
                                else
                                    SOME hiddenValues
                            else
                                SOME values
                      | [] => raise LibBase.Impossible "key with empty values")
                    map
        in
            (scope - 1, map)
        end

    fun depth (scope, _) = scope

    fun nth ((scope, map), n) =
        let
            val scope = scope - n
        in
            M.mapPartial
                ((Option.map #2) o (List.find (fn (valueScope, _) => valueScope = scope)))
                map
        end

    fun depthOf ((scope, map), key) =
        (
            case M.find (map, key) of
               SOME ((scopeValue, _) :: _) => SOME (scope - scopeValue)
             | NONE                        => NONE
             | SOME []                     => raise LibBase.Impossible "key with empty values"
        )

    fun map f (scope, map) =
        let
            val map =
                M.map
                    (List.map (fn (scope, value) => (scope, f value)))
                    map
        in
            (scope, map)
        end

    fun mapi f (scope, map) =
        let
            val map =
                M.mapi
                    (fn (key, values) =>
                        List.map
                            (fn (scope, value) => (scope, f (key, value)))
                            values)
                    map
        in
            (scope, map)
        end
end

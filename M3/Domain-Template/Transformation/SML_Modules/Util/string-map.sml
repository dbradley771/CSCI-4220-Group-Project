structure StringMap =
struct
    local
        structure Map = SplayMapFn (type ord_key = string val compare = String.compare)
    in
        open Map

        fun insertList (map, list) = List.foldl insert' map list
        fun fromList list = insertList (empty, list)
    end
end

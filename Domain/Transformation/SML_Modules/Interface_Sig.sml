
(* ------------------------------------------------------------------------------------------- *)
signature TL_INTERFACE_SIG =
sig

        (* The structure is: (function_id_string, function implementation in SML) list *)
        val functions : (
                            string
                            *
                            (
                                (ABSTRACT_REPRESENTATION.EXPR list * ABSTRACT_REPRESENTATION.EXPR list) list
                                ->
                                ABSTRACT_REPRESENTATION.EXPR
                            )
                        ) list;
end;
(* ------------------------------------------------------------------------------------------- *)

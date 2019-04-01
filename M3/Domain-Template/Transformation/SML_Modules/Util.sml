(* ------------------------------------------------------------------------------------------- *)
structure Util =
struct

(* ------------------------------------------------------------------------------------------- *)
fun printerr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)

(* ------------------------------------------------------------------------------------------- *)
fun member( id1, (id2, _) :: rest) = (id1 = id2) orelse member(id1, rest)
  | member( _, [] )                = false;
     
fun checkUnique( (x_str, x_fun) :: rest ) = if member(x_str, rest) then raise General.Fail("Error in UserDefined: The set of TL exported functions contains the duplicate name: " ^ x_str ^ "\n") 
                                            else checkUnique(rest)
    | checkUnique( [] ) = true                                                

(* ------------------------------------------------------------------------------------------- *)
fun createFolderForFile fileName = 
    let
        val path = String.tokens (fn #"/"=>true | #"\\"=>true | _=>false) fileName
         fun dropLast xs =
            let
                val len = List.length xs
                val len = if len > 0 then len - 1 else len
            in
                List.take (xs, len)
            end
        val folders = dropLast path
        
        fun createFolder ([],_) = ()
          | createFolder (f::xs, acc) = 
                let 
                    val name = (if acc = "" then f else acc ^ "/" ^ f) 
                in 
                    (OS.FileSys.mkDir(name); createFolder(xs, name)) handle OS.SysErr _ => createFolder(xs, name)
                end
    in
        createFolder(folders, "")
    end


(* ------------------------------------------------------------------------------------------- *)    
type internal_tl_type = (ABSTRACT_REPRESENTATION.EXPR list * ABSTRACT_REPRESENTATION.EXPR list) list

(** Executes a given function on a given argument and returns a constant. *)
fun execVoid f (arg : internal_tl_type ) =
    (
        f arg;
        ABSTRACT_REPRESENTATION.TRUE
    )

(* ------------------------------------------------------------------------------------------- *)
fun multiFileInput [tgt_file] = Input_Output.multiFileInput (tgt_file)
  | multiFileInput _          = raise General.Fail ("Error in Util.multiFileInput: inappropriate arguments to function.\n");

(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
(*                                 Exported Function List                                      *)
(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
   val functions = 

    [
        ("multiFileInput"   , multiFileInput        )
    ]

(* ------------------------------------------------------------------------------------------- *)    
end (* struct *)
(* ------------------------------------------------------------------------------------------- *)
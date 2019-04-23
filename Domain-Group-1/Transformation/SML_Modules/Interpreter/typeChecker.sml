(* =========================================================================================================== *)
structure TypeChecker =
struct

open Model;
open CONCRETE_REPRESENTATION;

(* =========================================================================================================== *)
(*
    Here is where your typeCheck and typeOf definitions go. The primary challenge here is to translate the parse 
    expression notation we used in M2 to the actual SML tree patterns used in the TL System. See the comments in
    the semantics.sml file for a more detailed discussion on this topic. 
*)


fun typeCheck( itree(inode("prog",_), [ stmt_list ] ), m) = m
  | typeCheck( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  | typeCheck _ = raise Fail("Error in Model.typeCheck - this should never occur")


(* =========================================================================================================== *)  
end (* struct *)
(* =========================================================================================================== *)









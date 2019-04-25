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

exception model_error;

fun typeOf( itree(inode("expr",_),_), m) = BOOL

  | typeOf( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeOf root = " ^ x_root ^ "\n\n")
  
  | typeOf _ = raise Fail("Error in Model.typeOf - this should never occur")

fun typeCheck( itree(inode("prog",_), [ stmt_list ] ), m) = typeCheck(stmt_list, m)

  (* Statement List *)
  | typeCheck( itree(inode("stmt_list",_),
                        [
                            stmt,
                            itree(inode(";",_), [] ),
                            stmt_list
                        ]
                    ),
                m0
            ) = let
                  val m1 = typeCheck(stmt, m0)
                  val m2 = typeCheck(stmt_list, m1)
                in
                  m2
                end
                
    | typeCheck( itree(inode("stmt_list",_),
                        [
                            stmt,
                            itree(inode(";",_), [] )
                        ]
                    ),
                m
            ) = typeCheck(stmt, m)
            
  (* Statement *)
  | typeCheck( itree(inode("stmt",_),
                [
                    stmt
                ]
            ),
        m
    ) = typeCheck(stmt, m)
  
  (* Declaration *)
  | typeCheck( itree(inode("declaration",_),
                [
                    itree(inode("int",_), [] ),
                    id_node
                ]
            ),
        m
    ) = let
          val id = getLeaf(id_node)
          val (_,n,_) = m
        in
            updateEnv(id, INT, n, m)
        end
    
  | typeCheck( itree(inode("declaration",_),
                [
                    itree(inode("bool",_), [] ),
                    id_node
                ]
            ),
        m
    ) = let
          val id = getLeaf(id_node)
          val (_,n,_) = m
        in
            updateEnv(id, BOOL, n, m)
        end
  
  (* Assignment *)
  | typeCheck( itree(inode("assignment",_),
                [
                    pre_post
                ]
            ),
        m
    ) = let
            val t1 = typeOf(pre_post, m)
        in
          if t1 = ERROR then raise model_error
          else m
        end
    
  | typeCheck( itree(inode("assignment",_),
                [
                    itree(inode("bool",_), [] ),
                    id_node,
                    itree(inode("=",_), [] ),
                    expr
                ]
            ),
        m0
    ) = let
          val id = getLeaf(id_node)
          val t = typeOf(expr, m0)
          val (_,n,_) = m0
          val m1 = updateEnv(id, BOOL, n, m0)
        in
          if t = BOOL then m1
          else raise model_error
        end
    
  | typeCheck( itree(inode("assignment",_),
                [
                    itree(inode("int",_), [] ),
                    id_node,
                    itree(inode("=",_), [] ),
                    expr
                ]
            ),
        m0
    ) = let
          val id = getLeaf(id_node)
          val t = typeOf(expr, m0)
          val (_,n,_) = m0
          val m1 = updateEnv(id, INT, n, m0)
        in
          if t = INT then m1
          else raise model_error
        end
   
  | typeCheck( itree(inode("assignment",_),
                [
                    id_node,
                    itree(inode("=",_), [] ),
                    expr
                ]
            ),
        m
    ) = let
          val id = getLeaf(id_node)
          val t1 = typeOf(expr, m)
          val t2 = getType(accessEnv(id, m))
        in
          if t1 = t2 andalso t1 <> ERROR then m
          else raise model_error
        end
  
  | typeCheck( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  
  | typeCheck _ = raise Fail("Error in Model.typeCheck - this should never occur")


(* =========================================================================================================== *)  
end (* struct *)
(* =========================================================================================================== *)









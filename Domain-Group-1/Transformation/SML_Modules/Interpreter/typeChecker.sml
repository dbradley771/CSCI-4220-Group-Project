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


fun typeOf( itree(inode("expr",_), [ logic_or ] ), m) = typeOf(logic_or, m)

  (* Logical Or *)
  | typeOf( itree(inode("logic_or",_),
                [
                    logic_or,
                    itree(inode("or",_), [] ),
                    logic_and
                ]
            ),
        m
    ) = let
          val t1 = typeOf( logic_or, m )
          val t2 = typeOf( logic_and, m )
        in
          if t1 = t2 andalso t1 = BOOL then BOOL
          else ERROR
        end

  | typeOf( itree(inode("logic_or",_), [ logic_and ] ), m) = typeOf(logic_and, m)

  (* Logical And *)
  | typeOf( itree(inode("logic_and",_),
                [
                    logic_and,
                    itree(inode("and",_), [] ),
                    equality
                ]
            ),
        m
    ) = let
          val t1 = typeOf( logic_and, m )
          val t2 = typeOf( equality, m )
        in
          if t1 = t2 andalso t1 = BOOL then BOOL
          else ERROR
        end

  | typeOf( itree(inode("logic_and",_), [ equality ] ), m) = typeOf(equality, m)

  (* Equality *)
  | typeOf( itree(inode("equality",_),
                [
                    equality,
                    itree(inode("==",_), [] ),
                    inequality
                ]
            ),
        m
    ) = let
          val t1     = typeOf( equality, m )
          val t2     = typeOf( inequality, m )
        in
          if t1 = t2 andalso t1 <> ERROR then BOOL
          else ERROR
        end
        
  | typeOf( itree(inode("equality",_),
                [
                    equality,
                    itree(inode("!=",_), [] ),
                    inequality
                ]
            ),
        m
    ) = let
          val t1 = typeOf( equality, m )
          val t2 = typeOf( inequality, m )
        in
          if t1 = t2 andalso t1 <> ERROR then BOOL
          else ERROR
        end

  | typeOf( itree(inode("equality",_), [ inequality ] ), m) = typeOf(inequality, m)

  (* Inequality *)
  | typeOf( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode("<",_), [] ),
                    additive2
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive1, m )
          val t2 = typeOf( additive2, m )
        in
          if t1 = t2 andalso t1 = INT then BOOL
          else ERROR
        end

  | typeOf( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode("<=",_), [] ),
                    additive2
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive1, m )
          val t2 = typeOf( additive2, m )
        in
          if t1 = t2 andalso t1 = INT then BOOL
          else ERROR
        end

  | typeOf( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode(">",_), [] ),
                    additive2
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive1, m )
          val t2 = typeOf( additive2, m )
        in
          if t1 = t2 andalso t1 = INT then BOOL
          else ERROR
        end

  | typeOf( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode(">=",_), [] ),
                    additive2
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive1, m )
          val t2 = typeOf( additive2, m )
        in
          if t1 = t2 andalso t1 = INT then BOOL
          else ERROR
        end

  | typeOf( itree(inode("inequality",_), [ additive ] ), m) = typeOf(additive, m)

  (* Additive *)
  | typeOf( itree(inode("additive",_),
                [
                    additive,
                    itree(inode("+",_), [] ),
                    multiplicative
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive, m )
          val t2 = typeOf( multiplicative, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("additive",_),
                [
                    additive,
                    itree(inode("-",_), [] ),
                    multiplicative
                ]
            ),
        m
    ) = let
          val t1 = typeOf( additive, m )
          val t2 = typeOf( multiplicative, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("additive",_), [ multiplicative ] ), m) = typeOf(multiplicative, m)
  
  (* Multiplicative *)
  | typeOf( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("*",_), [] ),
                    unary_minus
                ]
            ),
        m
    ) = let
          val t1 = typeOf( multiplicative, m )
          val t2 = typeOf( unary_minus, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("div",_), [] ),
                    unary_minus
                ]
            ),
        m
    ) = let
          val t1 = typeOf( multiplicative, m )
          val t2 = typeOf( unary_minus, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("mod",_), [] ),
                    unary_minus
                ]
            ),
        m
    ) = let
          val t1 = typeOf( multiplicative, m )
          val t2 = typeOf( unary_minus, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("multiplicative",_), [ unary_minus ] ), m) = typeOf(unary_minus, m)

  (* Unary Minus *)
  | typeOf( itree(inode("unary_minus",_),
                [
                    itree(inode("-",_), [] ),
                    unary_minus
                ]
            ),
        m
    ) = let
          val t = typeOf( unary_minus, m )
        in
          if t = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("unary_minus",_), [ exponent ] ), m) = typeOf(exponent, m)
  
  (* Exponent *)
  | typeOf( itree(inode("exponent",_),
                [
                    operations,
                    itree(inode("^",_), [] ),
                    exponent
                ]
            ),
        m
    ) = let
          val t1 = typeOf( operations, m )
          val t2 = typeOf( exponent, m )
        in
          if t1 = t2 andalso t1 = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("exponent",_), [ operations ] ), m) = typeOf(operations, m)
  
  (* Operations *)
  | typeOf( itree(inode("operations",_),
                [
                    operation
                ]
            ),
        m
    ) = typeOf(operation, m)

  | typeOf( itree(inode("operations",_),
                [
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] )
                ]
            ),
        m
    ) = typeOf(expr, m)

  | typeOf( itree(inode("operations",_),
                [
                    itree(inode("|",_), [] ),
                    expr,
                    itree(inode("|",_), [] )
                ]
            ),
        m
    ) = let
          val t = typeOf( expr, m )
        in
          if t = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("operations",_),
                [
                    itree(inode("not",_), [] ),
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] )
                ]
            ),
        m
    ) = let
          val t = typeOf( expr, m )
        in
          if t = BOOL then BOOL
          else ERROR
        end
  
  (* Identifier *)
  | typeOf( itree(inode("identifier",_),
                [
                    id_node
                ]
            ),
        m
    ) = getType(accessEnv(getLeaf(id_node), m))

  (* Value *)
  | typeOf( itree(inode("value",_),
                [
                    itree(inode("true",_), [] )
                ]
            ),
        m
    ) = BOOL
        
  | typeOf( itree(inode("value",_),
                [
                    itree(inode("false",_), [] )
                ]
            ),
        m
    ) = BOOL

  | typeOf( itree(inode("value",_),
                [
                    integer
                ]
            ),
        m
    ) = INT

  (* Pre_post *)
  | typeOf( itree(inode("pre_post",_), 
                [ 
                    itree(inode("++",_), [] ),
                    id_node
                ] 
             ), 
        m
    ) = let
          val t = getType(accessEnv(getLeaf(id_node), m))
        in
          if t = INT then INT
          else ERROR
        end

  | typeOf(  itree(inode("pre_post",_), 
                [ 
                    itree(inode("--",_), [] ),
                    id_node
                ] 
             ), 
        m
   ) = let
          val t = getType(accessEnv(getLeaf(id_node), m))
        in
          if t = INT then INT
          else ERROR
        end

  | typeOf(  itree(inode("pre_post",_), 
                [ 
                    id_node,
                    itree(inode("++",_), [] )
                ] 
             ), 
        m
    ) = let
          val t = getType(accessEnv(getLeaf(id_node), m))
        in
          if t = INT then INT
          else ERROR
        end

  | typeOf( itree(inode("pre_post",_), 
                [ 
                    id_node,
                    itree(inode("--",_), [] )
                ] 
             ), 
        m
    ) = let
          val t = getType(accessEnv(getLeaf(id_node), m))
        in
          if t = INT then INT
          else ERROR
        end
  
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
   
  (* print *) 
  | typeCheck(itree(inode("print_stmt", _),
                    [
                        itree(inode("print",_), []),
                        itree(inode("(",_), [] ),
                        expr,
                        itree(inode(")",_), [] )
                    ]
                ),
                m0
        ) = let
                val t1 = typeOf( expr, m0 )
            in
                if t1 = ERROR then raise model_error
                else m0
            end
            
  (* conditional *)
  | typeCheck(itree(inode("conditional",_),
                    [
                        conditional
                    ]
                ),
            m
        ) = typeCheck(conditional, m)
  
  (* if *)
  | typeCheck(itree(inode("if_stmt", _),
                    [
                    itree(inode("if",_), [] ),
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] ),
                    itree(inode("then",_), []),
                    block1
                    ]
                 ),
              m
           ) = let
                  val t1 = typeOf( expr, m )
                  val m1 = typeCheck( block1, m )
               in
                  if t1 = BOOL then m
                  else raise model_error
               end
               
  (* if else *)
  | typeCheck(itree(inode("if_else", _),
                  [
                      itree(inode("if",_), [] ),
                      itree(inode("(",_), [] ),
                      expr,
                      itree(inode(")",_), [] ),
                      itree(inode("then",_), []),
                      block1,
                      itree(inode("else",_), []),
                      block2
                  ]
               ),
              m
          ) = let
                  val t1 = typeOf( expr, m )
                  val m1 = typeCheck( block1, m )
                  val m2 = typeCheck( block2, m )
              in
                 if t1 = BOOL then m
                 else raise model_error
              end
  (* Block *)            
  | typeCheck(itree(inode("block",_), 
                [
                    itree(inode("{",_), []),
                    stmtList,
                    itree(inode("}",_), [])
                ]
            ),
          m0
      ) = let
            val m1 = typeCheck( stmtList, m0 )
          in
            m0
          end
   
   | typeCheck(itree(inode("loop",_),
                    [
                        loop
                    ]
            ),
          m
        ) = typeCheck(loop, m)
        
   | typeCheck (itree(inode("for_loop",_),
                [
                  itree(inode("for",_), [] ),
                  itree(inode("(",_), [] ),
                  assignment1,
                  itree(inode(";",_), [] ),
                  expr,
                  itree(inode(";",_), [] ),
                  assignment2,
                  itree(inode(")",_), [] ),
                  block
                ]
            ),
         m0
        ) = let
                val m1 = typeCheck( assignment1, m0 )
                val t1 = typeOf( expr, m1 )
                val m2 = typeCheck( block, m1 )
                val m3 = typeCheck( assignment2, m2 )
            in
                if t1 = BOOL then m0
                else raise model_error
            end
            
  | typeCheck(itree(inode("while_loop",_),
                [
                    itree(inode("while",_), [] ),
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] ),
                    block
                ]
            ),
        m0
     ) = let
            val t1     = typeOf( expr, m0 )
            val m1     = typeCheck( block, m0 )
        in
            if t1 = BOOL then m0
        else raise model_error
        
        end


            
            

  | typeCheck( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  
  | typeCheck _ = raise Fail("Error in Model.typeCheck - this should never occur")


(* =========================================================================================================== *)  
end (* struct *)
(* =========================================================================================================== *)









(* =========================================================================================================== *)
structure Semantics =
struct


(* This makes contents of the Model structure directly accessible (i.e., the prefix "Model." is not needed. *)            
open Model; 
            
(* This makes the internal representation of parse trees directly accessible. *)            
open CONCRETE_REPRESENTATION;

(* The following tree structure, defined in the CONCERETE_REPRESENTATION structure, is used in the TL System:

    datatype NODE_INFO = info of { id : IntInf.int, line : int * int , column : int * int, label : string };
	datatype INODE = inode of string * NODE_INFO
	                 | ...  
															
	datatype ITREE = itree of INODE * ITREE list;
*)


(* =========================================================================================================== *)
(* Here is where you add the M and E (as well as any other) definitions you developed in M2. The primary challenge here
   is to translate the parse expression notation we used in M2 to the actual SML tree patterns used in the TL System. 
   
   Example:
   
   M1: <stmtList> ::= <stmt> ";" <stmList>
   
   M2: M( [[ stmt_1 ; stmtList_1 ]], m) = M(stmtList_1, M(stmt_1,m))
    
   M4: 
        M( itree(inode("stmtList",_),
                    [
                        stmt,       (* this is a regular variable in SML and has no other special meaning *)
                        semiColon,  (* this is a regular variable in SML and has no other special meaning *)
                        stmtList    (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        
        Note that the above M4 implementation will match ANY tree whose root is "stmtList" having three children.
        Such matches can be further constrained by explicitly exposing more of the tree structure.
        
        M( itree(inode("stmtList",_),
                    [
                        stmt,                       (* this is a regular variable in SML and has no other special meaning *)
                        itree(inode(";",_), [] ),   (* A semi-colon is a leaf node. All leaf nodes have an empty children list. *)
                        
                        stmtList                    (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        Note that the above M4 implementation will match ANY tree satisifying the following constraints:
            (1) the root is "stmtList"
            (2) the root has three children
            (3) the second child is a semi-colon   
*)

(* Helper functions *)
fun pow(x, 0) = 1
  | pow(x, n) = x * pow(x, n-1);

(* Expressions *)
fun E( itree(inode("expr",_),
                [
                    logic_or
                ]
            ), 
        m
    ) = E(logic_or, m)
 
  (* Logical Or *)
  | E( itree(inode("logic_or",_),
                [
                    logic_or,
                    itree(inode("or",_), [] ),
                    logic_and
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(logic_or, m0)
        in
          if toBool(v1) then (Boolean true, m1)
          else
            let
              val (v2,m2) = E(logic_and,m1)
            in
              (Boolean(toBool(v1) orelse toBool(v2)), m2)
            end
        end
  
  | E( itree(inode("logic_or",_),
                [
                    logic_and
                ]
            ),
        m
    ) = E(logic_and, m)
  
  (* Logical And *)
  | E( itree(inode("logic_and",_),
                [
                    logic_and,
                    itree(inode("and",_), [] ),
                    equality
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(logic_and, m0)
        in
          if toBool(v1) then
            let
              val (v2,m2) = E(equality,m1)
            in
              (Boolean(toBool(v1) andalso toBool(v2)), m2)
            end
          else (Boolean false, m1)
        end
    
  | E( itree(inode("logic_and",_),
                [
                    equality
                ]
            ),
        m
    ) = E(equality, m)

  (* Equality *)
  | E( itree(inode("equality",_),
                [
                    equality,
                    itree(inode("==",_), [] ),
                    inequality
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(equality, m0)
          val (v2, m2) = E(inequality, m1)
        in
            (Boolean(v1 = v2), m2)
        end
        
  | E( itree(inode("equality",_),
                [
                    equality,
                    itree(inode("!=",_), [] ),
                    inequality
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(equality, m0)
          val (v2,m2) = E(inequality, m1)
        in
          (Boolean(v1 <> v2), m2)
        end
        
  | E( itree(inode("equality",_),
                [
                    inequality
                ]
            ),
        m
    ) = E(inequality, m)
        
  (* Inequality *)
  | E( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode("<",_), [] ),
                    additive2
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive1, m0)
          val (v2,m2) = E(additive2, m1)
        in
          (Boolean(toInt(v1) < toInt(v2)), m2)
        end
        
  | E( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode("<=",_), [] ),
                    additive2
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive1, m0)
          val (v2,m2) = E(additive2, m1)
        in
          (Boolean(toInt(v1) <= toInt(v2)), m2)
        end
  
  | E( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode(">",_), [] ),
                    additive2
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive1, m0)
          val (v2,m2) = E(additive2, m1)
        in
          (Boolean(toInt(v1) > toInt(v2)), m2)
        end
        
  | E( itree(inode("inequality",_),
                [
                    additive1,
                    itree(inode(">=",_), [] ),
                    additive2
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive1, m0)
          val (v2,m2) = E(additive2, m1)
        in
          (Boolean(toInt(v1) >= toInt(v2)), m2)
        end
  
  | E( itree(inode("inequality",_),
                [
                    additive
                ]
            ),
        m
    ) = E(additive, m)
  
  (* Additive *)
  | E( itree(inode("additive",_),
                [
                    additive,
                    itree(inode("+",_), [] ),
                    multiplicative
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive, m0)
          val (v2,m2) = E(multiplicative, m1)
        in
          (Integer(toInt(v1) + toInt(v2)), m2)
        end
   
   | E( itree(inode("additive",_),
                [
                    additive,
                    itree(inode("-",_), [] ),
                    multiplicative
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(additive, m0)
          val (v2,m2) = E(multiplicative, m1)
        in
          (Integer(toInt(v1) - toInt(v2)), m2)
        end
        
  | E( itree(inode("additive",_),
                [
                    multiplicative
                ]
            ),
        m
    ) = E(multiplicative, m)
  
  (* Multiplicative *)  
  | E( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("*",_), [] ),
                    unary_minus
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(multiplicative, m0)
          val (v2,m2) = E(unary_minus, m1)
        in
          (Integer(toInt(v1) * toInt(v2)), m2)
        end
   
  | E( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("div",_), [] ),
                    unary_minus
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(multiplicative, m0)
          val (v2,m2) = E(unary_minus, m1)
        in
          (Integer(toInt(v1) div toInt(v2)), m2)
        end
  
  | E( itree(inode("multiplicative",_),
                [
                    multiplicative,
                    itree(inode("mod",_), [] ),
                    unary_minus
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(multiplicative, m0)
          val (v2,m2) = E(unary_minus, m1)
        in
          (Integer(toInt(v1) mod toInt(v2)), m2)
        end
  
  | E( itree(inode("multiplicative",_),
                [
                    unary_minus
                ]
            ),
        m
    ) = E(unary_minus, m)
    
  (* Unary Minus *)
  | E( itree(inode("unary_minus",_),
                [
                    itree(inode("-",_), [] ),
                    unary_minus
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(unary_minus, m0)
        in
          (Integer(~(toInt(v1))), m1)
        end
  
  | E( itree(inode("unary_minus",_),
                [
                    exponent
                ]
            ),
        m
    ) = E(exponent, m)
  
  (* Exponent *)
  | E( itree(inode("exponent",_),
                [
                    operations,
                    itree(inode("^",_), [] ),
                    exponent
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(exponent, m0)
          val (v2,m2) = E(operations, m1)
        in
          (Integer(pow(toInt(v1),toInt(v2))), m2)
        end
  
  | E( itree(inode("exponent",_),
                [
                    operations
                ]
            ),
        m
    ) = E(operations, m)
  
  (* Operations *)
  | E( itree(inode("operations",_),
                [
                    operation
                ]
            ),
        m
    ) = E(operation, m)
  
  | E( itree(inode("operations",_),
                [
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] )
                ]
            ),
        m
    ) = E(expr, m)
  
  | E( itree(inode("operations",_),
                [
                    itree(inode("|",_), [] ),
                    expr,
                    itree(inode("|",_), [] )
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(expr, m0)
        in
          (Integer(Int.abs(toInt(v1))), m1)
        end
  
  | E( itree(inode("operations",_),
                [
                    itree(inode("not",_), [] ),
                    itree(inode("(",_), [] ),
                    expr,
                    itree(inode(")",_), [] )
                ]
            ),
        m0
    ) = let
          val (v1,m1) = E(expr, m0)
        in
          (Boolean(not(toBool(v1))), m1)
        end
  
  (* Identifier *)
  | E(itree(inode("identifier",_),
                [
                    id_node
                ]
            ),
        m
    ) = let
          val id = getLeaf(id_node)
          val loc = getLoc(accessEnv(id, m))
          val v = accessStore(loc, m)
        in
          (v, m)
        end
  
  (* Value *)
  | E( itree(inode("value",_),
                [
                    itree(inode("true",_), [] )
                ]
            ),
        m
    ) = (Boolean true, m)
    
  | E( itree(inode("value",_),
                [
                    itree(inode("false",_), [] )
                ]
            ),
        m
    ) = (Boolean false, m)
  
  | E( itree(inode("value",_),
                [
                    integer
                ]
            ),
        m
    ) = let
          val v = getLeaf(integer)
          val v_int = valOf(Int.fromString(v))
        in
          (Integer v_int, m)
        end
  
  (* Pre_post *)
  | E( itree(inode("pre_post",_), 
                [ 
                    itree(inode("++",_), [] ),
                    id_node
                ] 
             ), 
        m0
    ) = let
          val id = getLeaf(id_node)
          val loc = getLoc(accessEnv(id, m0))
          val v = accessStore(loc, m0)
          val inc = Integer(toInt(v) + 1)
          val m1 = updateStore(loc, inc, m0)
        in
          (inc, m1)
        end
        
  | E(  itree(inode("pre_post",_), 
                [ 
                    itree(inode("--",_), [] ),
                    id_node
                ] 
             ), 
        m0
    ) = let
          val id = getLeaf(id_node)
          val loc = getLoc(accessEnv(id, m0))
          val v = accessStore(loc, m0)
          val dec = Integer(toInt(v) - 1)
          val m1 = updateStore(loc, dec, m0)
        in
          (dec, m1)
        end
        
  | E(  itree(inode("pre_post",_), 
                [ 
                    id_node,
                    itree(inode("++",_), [] )
                ] 
             ), 
        m0
    ) = let
          val id = getLeaf(id_node)
          val loc = getLoc(accessEnv(id, m0))
          val v = accessStore(loc, m0)
          val inc = Integer(toInt(v) + 1)
          val m1 = updateStore(loc, inc, m0)
        in
          (v, m1)
        end
        
  | E(  itree(inode("pre_post",_), 
                [ 
                    id_node,
                    itree(inode("--",_), [] )
                ] 
             ), 
        m0
    ) = let
          val id = getLeaf(id_node)
          val loc = getLoc(accessEnv(id, m0))
          val v = accessStore(loc, m0)
          val dec = Integer(toInt(v) - 1)
          val m1 = updateStore(loc, dec, m0)
        in
          (v, m1)
        end
    
  | E(  itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn E root = " ^ x_root ^ "\n\n")
  
  | E _ = raise Fail("error in Semantics.E - this should never occur")




(* Model Functions *)
fun M(  itree(inode("prog",_), 
                [ 
                    stmt_list
                ] 
             ), 
        m
    ) = M( stmt_list, m )
    
  (* Statement List *)
  | M( itree(inode("stmt_list",_),
                [
                    stmt,
                    itree(inode(";",_), [] ),
                    stmt_list
                ]
            ),
        m0
    ) = let
          val m1 = M(stmt, m0)
          val m2 = M(stmt_list, m1)
        in
          m2
        end

  | M( itree(inode("stmt_list",_),
                [
                    stmt,
                    itree(inode(";",_), [] )
                ]
            ),
        m
    ) = M( stmt, m )
   
  (* Statement *)
  | M( itree(inode("stmt",_),
                [
                    stmt
                ]
            ),
        m
    ) = M(stmt, m)
  
  (* Declaration *)
  | M( itree(inode("declaration",_),
                [
                    itree(inode("int",_), [] ),
                    id_node
                ]
            ),
        (env, n, s)
    ) = let
          val id = getLeaf(id_node)
        in
            updateEnv(id, INT, n, (env, n, s))
        end
    
  | M( itree(inode("declaration",_),
                [
                    itree(inode("bool",_), [] ),
                    id_node
                ]
            ),
        (env, n, s)
    ) = let
          val id = getLeaf(id_node)
        in
            updateEnv(id, BOOL, n, (env, n, s))
        end
   
  (* Assignment *)
  | M( itree(inode("assignment",_),
                [
                    pre_post
                ]
            ),
        m0
    ) = let
            val (_, m1) = E(pre_post, m0)
        in
          m1
        end
    
  | M( itree(inode("assignment",_),
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
          val (v, m1) = E(expr, m0)
          val (_,n,_) = m1
          val m2 = updateEnv(id, BOOL, n, m1)
          val loc = getLoc(accessEnv(id, m2))
          val m3 = updateStore(loc, v, m2)
        in
          m3
        end
    
  | M( itree(inode("assignment",_),
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
          val (v, m1) = E(expr, m0)
          val (_,n,_) = m1
          val m2 = updateEnv(id, INT, n, m1)
          val loc = getLoc(accessEnv(id, m2))
          val m3 = updateStore(loc, v, m2)
        in
          m3
        end
   
  | M( itree(inode("assignment",_),
                [
                    id_node,
                    itree(inode("=",_), [] ),
                    expr
                ]
            ),
        m0
    ) = let
          val id = getLeaf(id_node)
          val (v, m1) = E(expr, m0)
          val loc = getLoc(accessEnv(id, m1))
          val m2 = updateStore(loc, v, m1)
        in
          m2
        end

  | M( itree(inode("print", _),
                [
                    id_node,
                    itree(inode("print",_), []),
                    expr
                ]
            ),
            m0
     ) = let
            val (v, m1) = E(expr, m0)
         in
            print(v1)
         end

  | M( itree(inode("conditional",_),
                   [
                       conditional
                   ]
               ),
           m
       ) = M(conditional, m)

  | M( itree(inode("if", _),
                [
                    id_node,
                    itree(inode("if",_), [] ),
                    expr
                    itree(inode("then",_), []),
                    block1
                ]
             ),
            m
      ) = let
               val (v, m1) = E(expr, m0)
           in
                if v then M( block1, m1)
                else m1
           end

  | M( itree(inode("if-else", _),
                  [
                      id_node,
                      itree(inode("if",_), [] ),
                      expr
                      itree(inode("then",_), []),
                      block1
                      itree(inode("else",_), []),
                      block2
                  ]
               ),
              m
        ) = let
                 val (v, m1) = E(expr, m0)
             in
                  if v then M( block1, m1)
                  else M(block2, m1)
             end

  | M( itree(inode("block", _),
                [
                    id_node,
                    itree(inode("{",_), []),
                    stmtList
                    itree(inode("}",_), [])
                ]
            ),
           (env0, n, s0)
      ) = let
            val (env1, n, s1) = M(stmtList, (env0, n, s0))
            val m2 = (env0, n, s1)
          in
            m2
          end

  | M( itree(inode( ""

  (* Print *)
  
  (* Conditional *)
  
  (* Loop *)
  | M( itree(inode("loop",_),
                [
                    loop
                ]
            ),
        m
    ) = M(loop, m)
  
  (* For Loop *)
  | M( itree(inode("for_loop",_),
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
          val m1 = M(assignment1, m0)
          
          fun aux(m2) = 
            let
                val (v, m3) = E(expr, m2)
            in
                if toBool(v) then
                    let
                        val m4 = M(block, m3)
                        val m5 = M(assignment1, m2)
                        val m6 = aux(m5)
                    in
                        m6
                    end
                else m3
            end
            
        in
          aux(m1)
        end

  (* While Loop *)
  | M( itree(inode("while_loop",_),
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
    
          fun aux(m1) = 
            let
                val (v, m2) = E(expr, m1)
            in
                if toBool(v) then
                    let
                        val m3 = M(block, m2)
                        val m4 = aux(m3)
                    in
                        m4
                    end
                else m2
            end
            
        in
          aux(m0)
        end

  | M(  itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  
  | M _ = raise Fail("error in Semantics.M - this should never occur")


(* =========================================================================================================== *)
end (* struct *)
(* =========================================================================================================== *)









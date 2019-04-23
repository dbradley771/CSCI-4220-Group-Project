exception runtime_error;

(* =========================================================================================================== *)
structure Model =

struct 

(* =========================================================================================================== *)
(* This function may be useful to get the leaf node of a tree -- which is always a string (even for integers).
   It is up to the interpreter to translate values accordingly (e.g., string to integer and string to boolean).
   
   Consult (i.e., open Int and open Bool) the SML structures Int and Bool for functions that can help with 
   this translation. 
*)
(* fun getLeaf( term ) = CONCRETE.leavesToStringRaw term *)

fun error msg = ( print msg; raise runtime_error );



(* For your typeChecker you may want to have a datatype that defines the types 
  (i.e., integer, boolean and error) in your language. *)
datatype types = INT | BOOL | ERROR;


(* It is recommended that your model store integers and Booleans in an internal form (i.e., as terms belonging to
   a userdefined datatype (e.g., denotable_value). If this is done, the store can be modeled as a list of such values.
*)
datatype denotable_value =  Boolean of bool 
                          | Integer of int;


type loc   = int
type env   = (string * types * loc) list
type store = (loc * denotable_value) list


(* Functions used to interact with the model *)
fun accessEnv ( id1, (env,_,s) ) = 
       let
          val msg = "Error: accessEnv " ^ id1 ^ " not found.";

          fun aux [] = error msg
            | aux ((id,t,loc)::env) = 
                     if id1 = id then (t,loc)
                     else aux env;
       in
          aux env
       end;
 
(* fun accessStore *)
fun accessStore ( loc1, (env,_,s) ) = 
       let
          val msg = "Error: accessStore " ^ Int.toString(loc1) ^ " not found.";

          fun aux [] = error msg
            | aux ((loc,v)::s) = 
                     if loc1 = loc then v
                     else aux s;
       in
          aux s
       end;

fun updateEnv ( id1, t1, loc1, (env1,n1,s) ) =
        let
          fun aux [] = ( [(id1, t1, loc1)], n1+1,s )
            | aux ((id,t,loc)::env2) =
                if id1 = id then ( (id1, t1, loc1)::env2, n1, s)
                else
                    let
                      val (env3,n2,_) = aux env2
                    in
                      ( (id,t,loc)::env3, n2, s )
                    end
        in
          aux env1
        end;
 
fun updateStore ( loc1, v1, (env,n,s1) ) =
        let
          fun aux [] = [(loc1, v1)]
            | aux ((loc,v)::s2) =
                if loc1 = loc then (loc1, v1)::s2
                else (loc,v)::aux s2
        in
          ( env, n, aux s1 )
        end;
                
 
(* fun getLoc *)
 
(* fun getType *)


(* The model defined here is a triple consisting of an environment, an address counter, and a store. The environment
   and the store are lists similar to what we have used in class. The address counter serves as an implementation of
   new(). Note that, depending on your implementation, this counter either contains the address of (1) the
   next available memory location, or (2) the last used memory location -- it all depends on when the counter is 
   incremented. *)
val initialModel = ( []:env, 0:loc, []:store )

(* =========================================================================================================== *)
end; (* struct *) 
(* =========================================================================================================== *)

(* Tests *)
open Model;

(* (accessEnv("x", ([("x", INT, 0)], 1, [])); *)
(* accessEnv("x", initialModel); *)

(* accessStore(0, ([("x", INT, 0)], 1, [(0, Integer 8)]) );
accessStore(2, ([("x", BOOL, 0),("y", INT, 1),("z", BOOL, 2)], 3, [(0, Boolean true),(1, Integer 3),(2, Boolean false)]));
accessStore(0, ([("x", INT, 0)], 1, [])); *)

(* val (_,newLoc,_) = initialModel
val m1 = updateEnv("x", INT, newLoc, initialModel);
val (_,newLoc,_) = m1
val m2 = updateEnv("y", BOOL, newLoc, m1);
val m3 = updateEnv("x", BOOL, 3, m2); *)

(* val m1 = updateStore(0, Integer 5, initialModel);
val m2 = updateStore(1, Boolean true, m1);
val m3 = updateStore(0, Integer 7, m2); *)








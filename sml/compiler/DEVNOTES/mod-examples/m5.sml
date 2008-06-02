(* 
 * This example triggers an Lty normalization error
 * LtyNorm:Bad TC_ENV TC_VAR [Beta]
 * This message means that a TC_VAR is indexing a type in a beta binding 
 * that does not exist. 
 * It appears to test multiple references to a single formal parameter type
 * but in two separate parameter structures.  
 * This test case ensures that representative entpaths are those with truly
 * unique types (stamps) and not just unique paths. In this example, 
 * A.t and B.t are types with the same stamp but the entpaths are different
 * because of unique stamps generated for each of A and B. But the functor
 * application should only mention the single representative type A.t. 
 *)

signature PS = sig type t end  

functor MLRiscGenQ
   (structure A : PS 
    structure B : PS) = 
struct end 

functor MachineGen(X : PS) =
struct
   structure MLTreeGen =
      MLRiscGenQ
	  (structure A=X
           structure B=X)
end

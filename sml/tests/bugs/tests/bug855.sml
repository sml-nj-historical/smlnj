(* bug855.sml *)
(* 855. sharing in signatures *)

signature C1 = sig type R val foo : R -> R end;

(* this should be rejected, since sharing constraints on parameter appear
   in result signature *)
funsig Soo(structure T1:C1 structure T2:sig include C1 end) =
  sig include C1 sharing T1 = T2 end;

functor Foo(structure T1:C1 
            structure T2:sig include C1 sharing type R=T1.R end):
         sig include C1 sharing T1 = T2 end =
struct type R = T2.R val foo = T1.foo end;  
(* this error message could be clearer:
std_in:15.36-15.42 Error: definitional sharing constraint T1 = T2 can never be satisfied
*)

(* Make sure that there aren't compiler bugs involving the
   handling of error structures in open statements and in signature
   matching.  
*)

functor F(A:ZZ) : sig end = A;


(* 92.sml *)

signature S =
sig
  datatype d = D
end

structure E =
struct
  datatype d = D
end
      
functor F(structure A : S = E) = 
struct
  val x : E.d = A.D
end

(* bug209.2.sml *)
(* problem in eqtypes/defineEqTycon/eqty *)

signature S1 = sig type t end;
structure A = struct type t = int end;
functor F(X : S1)  =  
struct
  structure B = struct datatype s1 = K of X.t end
  datatype s = L of B.s1
end;
structure C = F(A);

fun f(x:C.s) = (x = x);  (* is C.s recognized as equality type? *)

(* bug209.1.sml *)
(* problem in eqtypes/defineEqTycon/eqty *)

signature S1 = sig type t end;
structure A = struct type t = int end;
functor F(X : S1)  =  
struct
  structure B = struct datatype s1 = K of X.t end
  type s = B.s1    (* reevaluated first, before B.s1 has been reevaluated *)
end;
structure C = F(A);

fun f(x:C.s) = (x = x);  (* is C.s recognized as equality type? *)

(* test6.sml *)
(* keywords: functor *)

(* bug97b -- originally from Tofte 
   tests reference to parameter in result signature *)

signature VAL = sig type value end;

functor F(X: VAL): sig val f: X.value -> unit end =
struct
  fun f(v)= ()
end;

structure M = F(struct type value = unit end);

M.f();

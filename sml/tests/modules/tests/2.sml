(* test2.sml *)
(* keywords: functor, open *)

functor F(X : sig end) = 
struct
  open X
end;

(* 232.sml *)
(* generativity of stamps *)

functor F(eqtype u functor G():sig type t val x:t sharing type t=u end) = 
struct
  structure a = G();
  structure b = G();
  val b = a.x = b.x
end;

functor F(functor G():sig type t val x:t end) = 
struct
  structure a = G();
  structure b = G();
  val b = a.x = b.x
end;


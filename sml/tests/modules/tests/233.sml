(* 233.sml *)
(* eqtype *)

functor F(type u functor G():sig type t = u val x:t end) = 
struct
  structure a = G();
  structure b = G();
  val b = a.x = b.x
end;


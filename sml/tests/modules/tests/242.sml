(* 242.sml *)

functor F(functor G():sig type t end) : sig type t end =
struct
  structure A = G() 
  type t = A.t
end;

structure A = F(functor G()=struct type t = int end);

val it = 3 : A.t;


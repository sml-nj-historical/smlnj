(* bug881.sml *)
(* 881. Higher order functors without origin's don't work. *)

functor F(functor G():sig type t end) : sig type t end =
struct
  structure A = G() 
  type t = A.t
end;

structure B = F(functor G()=struct type t = int end);

3 : B.t;

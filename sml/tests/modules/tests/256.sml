(* 256.sml *)
(* = b641.1.sml *)

signature SIG1 =
sig
  type t
  val x: t
end

signature SIG2 =
sig 
  functor Foo(X:SIG1) : sig val w: X.t end 
end

structure B : SIG2 =
struct
  functor Foo(X:SIG1) =
    struct
      val w = X.x 
    end
end

functor F = B.Foo

(*
structure XX = B.Foo(struct type t = int val x = 3 end);
or
functor F = B.Foo;
or
open B;
*)



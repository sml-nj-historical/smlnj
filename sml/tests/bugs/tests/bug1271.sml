(* bug1271.sml *)

signature SIG1 =
sig
  type t
  val x: t
end;

signature SIG2 =
sig 
  functor Foo(X:SIG1) : sig val w: X.t end 
end;

structure B : SIG2 =  (* sig constraint necessary *)
struct
  functor Foo(X:SIG1) =
    struct
      val w = X.x 
    end
end;

open B;

(* or some other reference to B.Foo, such as:
structure XX = B.Foo(struct type t = int val x = 3 end);

functor F = B.Foo;
*)

(* bug641.1.sml *)

signature SIG1 =
sig
  type t
  val x: t
end;

signature SIG2 =
sig 
  functor Foo(X:SIG1) : sig val w: X.t end 
end;

structure B : SIG2 =
struct
  functor Foo(X:SIG1) =
    struct
      val w = X.x 
    end
end;

open B;

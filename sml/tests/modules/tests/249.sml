signature SIG1 =
sig
  type t
  type u
  val x:t
  val y:u
end;

structure A: SIG1 =
struct
  type t=int
  type u=bool
  val x=5
  val y=true
end;

signature SIG2 =
sig 
  functor Foo(X:SIG1) : sig val z : A.t val w: X.t end 
end;

(* This was to test if the restriction on what names may be used -- relevant 
   for separate compilation -- was enforced in v88 *)

structure B : SIG2 =
struct
  functor Foo(X:SIG1) =
    struct
      val z = A.x + 1
      val w = X.x 
      type foo = bool 
    end
end;

(* This was to test if signature contraints of SIG2 were propagated down to the body
   of the functor. Pleasantly, it is *)

open B;

structure C = Foo(A);

structure D : SIG1 =
struct
  type t = bool
  type u = int
  val x = false
  val y = 7
end;

structure E = Foo(D);

C.w;
(* val it = 1 : A.t -- should have been equal to A.x = 5  !!!!! *)

A.x;
(* val it = 5 : A.t *)

C.z;
(* val it = 6 : A.t -- rightly so *)

E.z;
(* val it = 6 : A.t -- again rightly so *)

E.w; (* should have been false *)
(* val it = Error: Compiler bug: PrintVal.switch: none of the datacons matched *)

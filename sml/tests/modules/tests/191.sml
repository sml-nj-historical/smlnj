(* tests that all where defs are applied *)
(* derived from problem comping boot/Unix/unix.sml *)

signature S1 =
sig
  type w
  type r
  val f : w -> unit
end;

signature S3 =
sig
  type w
  val a : w
end;

functor F(X : S3) : S1 =
struct
  type w = X.w
  type r = unit
  fun f (x:w) = ()
end;

structure P : S3 =
struct
  datatype w = W
  val a : w = W
end;

structure T :> S1
    where type w = P.w
    and type r = unit
  = F(P);

val x = T.f P.a;

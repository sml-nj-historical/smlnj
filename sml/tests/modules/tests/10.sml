(* 10.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

signature FULLSET =
sig
  type elem
  val g: elem -> unit
end;

functor BS(B : sig type u end ) =
struct
  type e = B.u
  fun f x = ()
end;

functor FS (B : sig type u end) : FULLSET =  (* signature FULLSET caused bug *)
struct
  structure C = BS(B)
  type elem = C.e
  val g = C.f
end;

structure E = FS(struct type u = int list end);

fun ff (x: E.elem) = hd x;

fun h (y: int) = E.g [y];

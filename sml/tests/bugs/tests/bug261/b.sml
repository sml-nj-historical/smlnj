(*------------------------ b.sml ---------------------------*)

signature BSIG =
sig
  type t
  val x : t
end;

functor BF() : BSIG =
struct
  type t = int
  val x = 3
end;

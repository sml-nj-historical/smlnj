(* bug749.sml *)
(* 749. nonequality type identified as equality type *)

signature EQTYPE  =
sig
  type T
  val eq : T * T -> bool
end;

structure S: EQTYPE =
struct
  type T = int -> int
  fun eq _ = false
end;

functor ProductEqTypeFUN (structure t1 : EQTYPE and t2 : EQTYPE) 
			: EQTYPE =
struct
  type T = t1.T * t2.T
  val eq = op=   (* this should not work because '=' not defined *)
end;

structure SS = ProductEqTypeFUN (structure t1 = S  structure t2 = S);

fun f x = x + 1;
fun g x = x - 1;

(* now SS.eq((f, f), (f, f)) yields true and
SS.eq((f, f), (g, g)) yields false. *)

(* bug1409.1.sml *)

functor T () =
struct
  val g = 1.0
  val f = 2.0
  val m = 3.0
  val p = 4.0
  val q = 5.0
  val u = 6.0
  val h = 7.0
end;

structure E =
struct

  datatype t = A | B

  fun b _ = B

  val s = b()

  structure C = T ()

  val {g,f,m,h,p,q,u} =
      case s
	of A =>
	   {g = C.g, f = C.f, m = C.m, h = C.h, p = C.p, q = C.q, u = C.u}
	| _ => 
	   {g = C.g, f = C.f, m = C.m, h = C.h, p = C.p, q = C.q, u = C.u}
end;

val test =
    ((E.g , 1.0),(E.f , 2.0),(E.m , 3.0),(E.p ,4.0),(E.q , 5.0),(E.u , 6.0),
     (E.h , 7.0 ));

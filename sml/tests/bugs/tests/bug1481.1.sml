(* bug1481.1.sml *)

signature SIG =
sig
  val f : int -> int
end;
   
functor Fun2() =
struct
 structure F : SIG = Fun1()
end;
   
structure S = Fun2();

(* bug852.sml *)
(* 852. include merging types *)

signature AM = sig type R val plus:R->R->R and zero:R end
signature MM = sig type R val times:R->R->R and one:R end
signature S1 = sig include AM MM end;

structure M:S1 =
struct
  type R=int
  fun plus(x:int)y=x+y val zero=0
  val one=1
  fun times(x:int)y=x*y
end;

signature S2 = sig include AM type R end;

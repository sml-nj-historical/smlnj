(* bug545.sml *)
(* signature matching looping? *)

signature S =
sig
  val f : 'b -> int
end;

structure S1:S =
struct
  fun f x = x
end;

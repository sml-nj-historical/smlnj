(* bug963.sml *)

signature l =
sig
  type i
  val k : unit -> i
  val j : i * i -> i
end;

signature k =
sig
  type i
  val k : unit -> int * i
  val j : i * i -> i
end;

signature p = sig include l include k end;

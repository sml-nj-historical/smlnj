(* bug954.sml *)

signature S1 =
sig
  type t = int
  val x:t
end;

signature S2 =
sig
  type t = string
end;

signature S3 =
sig
  include S2
  include S1
end;

structure M3 :> S3 = struct type t=real val x=3.4 end;
		      (* ACCEPTED! *)

M3.x ^ "Memory fault";

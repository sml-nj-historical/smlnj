(* bug1390.1.sml *)

signature X =
sig
  type i;
  type j;
  val f : i -> j;
end where type i = int

and Y =  (* and shifted with where type *)
sig
  type t;
end;  

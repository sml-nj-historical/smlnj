(* bug96.sml *)

signature Sigtest =
sig
  structure S:
   sig
     type t1
     val x:t1->t1
   end
  structure R:
   sig
     type t2
     val x:t2->t2
   end
  type t
  val f:t1->R.t2
end;


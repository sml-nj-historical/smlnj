(* bug1195.sml *)

structure A :
  sig
    eqtype vector
    type elem
    val app : elem -> unit -> vector -> unit
  end =
CharVector;

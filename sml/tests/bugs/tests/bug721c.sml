(* bug721c.sml *)
(* 721. Local weakness 0 typing rejected. *)

(* this should NOT work *)

let val f = !(ref(fn x =>x)) in f 3; f true end;

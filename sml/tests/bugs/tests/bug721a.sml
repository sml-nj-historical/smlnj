(* bug721a.sml *)
(* 721. Local weakness 0 typing rejected. *)

(* these should all work *)

let val f = !(ref(fn x =>x)) in 3 end;

let val f = !(ref(fn x =>x)) in f 3 end;

local val f = !(ref(fn x =>x)) in val y = 3 end;

local val f = !(ref(fn x =>x)) in val y = f 3 end;

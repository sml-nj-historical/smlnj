(* bug721d.sml *)

(* this should NOT work *)

local val f = !(ref(fn x =>x)) in val y = (f 3; f true) end;

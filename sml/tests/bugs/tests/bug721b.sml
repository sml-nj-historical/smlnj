(* bug721b.sml *)
(* 721. Local weakness 0 typing rejected. *)

(* this should NOT work *)

local structure Foo = struct val f = !(ref(fn x=> x)) end
in val y = 3 end;


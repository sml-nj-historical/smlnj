(* bug607.sml *)
(* weak typing failure *)

fn x => let val a = ref nil in
	(let val b = a in b := [true]; hd (!b) + 1; (fn z => z) end) () end;

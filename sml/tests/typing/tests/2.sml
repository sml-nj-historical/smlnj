(* 2.sml *)
(* keywords: ref, weak *)

val a = 
    let val foo = ref nil
     in (fn x as {...} => foo:=[x] | (y,z) => ();
	 foo)
    end



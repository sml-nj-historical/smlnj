(* bug224.sml *)

(* A weakness 0 type variable should be admitted, but not generalized, in local
   declarations. *)

let val f = ref(fn x => x) in f := not; !f true end;

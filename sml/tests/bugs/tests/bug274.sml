(* bug274.sml *)

val a = 
  let val foo = ref nil
  in
    (fn x as {...} => foo:=[x] | (y,z) => ();
     foo)
  end;



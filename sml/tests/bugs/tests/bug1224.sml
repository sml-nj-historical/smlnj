(* bug1224.sml *)

structure T =
struct
  datatype 'a pref = PREF of {obj : 'a}
  fun apply (PREF {obj}, arg) = obj arg

  fun f ((x, y), z) = y

  fun s () = apply (PREF {obj=f}, (("one", "two"), "three"))
  fun t () = apply (PREF {obj=f}, (("one", "two"), ()))
end;

T.s();

T.t();

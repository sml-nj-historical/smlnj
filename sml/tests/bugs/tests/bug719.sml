(* bug719.sml *)
(* 719. weakness zero polymorphism allowed in structures *)

structure Foo = struct
 val f = ! (ref (fn x => x))  (* erroneously given type '0Z -> '0Z *)
end

(* bug169.sml *)
(* inferring eqtypes in signatures *)

functor F() = struct abstype t = E with val mk_t = E end end;
structure f = F();
structure f = struct abstype t = E with val mk_t = E end end;

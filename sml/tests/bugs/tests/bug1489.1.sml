(* bug1489.1.sml *)

abstype t = Mkt of int
with val mk = Mkt
     and eq : t * t -> bool = op =
end;
val x = mk 3 and y = mk 4;
eq (x,y);

(* bug569.sml *)
(* type inference with flexible records *)


fun f x = let val y = #1 x val z = #2 x in (y, z, x:('a * 'b)) end;

f(1,0);			(* this should have no type variables in its type *)

(* bug1138.sml *)

functor F(structure T : SIGNOT_DEFINED) = 
struct
 fun test() = ()
end;

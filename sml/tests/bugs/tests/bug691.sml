(* bug691.sml *)
(* 691. Compiler bug: ModuleUtil: lookFormalBinding 1 *)

signature S1 = sig type a ; val intro : unit -> a end;

functor F4 
  (structure X1 : S1)
  (structure X2 : sig val x : X1.a end) =
struct end;

(* bug1317.sml *)
(* secondary error: Compiler bug: EntityEnv: lookEP.1 *)

functor F () : FSIG = struct end 
structure C =  F ()


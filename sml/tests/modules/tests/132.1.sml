(* 132.1.sml *)

signature SIG = sig type t = unit end;
functor F(S : SIG) = struct end;

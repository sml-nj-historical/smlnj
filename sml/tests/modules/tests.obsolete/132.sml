(* 132.sml *)

signature SIG = sig type t sharing type t = unit end;
functor F(S : SIG) = struct end;

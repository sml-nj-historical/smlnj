(* bug952.sml *)
(* unsound imperative types *)

local val ll = ref []
in functor F(type t val x:t) =
      struct val l = (ll := x :: (!ll); !ll)
	     (* here ll gets the type
		ll : t list ref
	     *)
      end 
end;

structure S1 = F(type t = int val x = 4);
structure S2 = F(type t = int->int val x = fn x => x);

hd(tl(S2.l)) 3   (* Applies an integer to an integer ... *)

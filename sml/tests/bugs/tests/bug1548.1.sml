(* bug1548.1.sml *)

functor F(V: sig
		type t
		val v: t
	     end): sig
		      type 'a u
		      val v: 'a u
		   end =
struct
  open V
  type 'a u = t
end;

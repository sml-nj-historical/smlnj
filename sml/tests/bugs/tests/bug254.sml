(* bug254.sml *)
(* failure to detect type error (in 0.60) *)

signature SIG = sig
  type EA
  val fopt : ((EA * EA) -> unit) option
end;

functor F(M: SIG) :  sig  end =
struct
    val g = 
	case M.fopt
	 of SOME f => let fun h x = f (x,x)
	      	       in h
		      end
    val x = g 2  (* this is a type error *)
end;

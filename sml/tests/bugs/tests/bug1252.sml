(* bug1252.sml *)

functor F(type t) =
  let structure S : sig
		       type v
		       val x : v
		    end =
	 struct
	    datatype 'a u = U of (t * 'a) list
	    type v = t u
	    val x = U[]
	 end
  in struct
	datatype t = T of S.v
	val x = T S.x
     end
  end;

structure S = F(type t = int);

(* bug 332 *)
structure S : 
    sig 
	type t 
	val F : unit -> t 
    end 
= struct
      datatype t = F
  end;
    
S.F ();
    

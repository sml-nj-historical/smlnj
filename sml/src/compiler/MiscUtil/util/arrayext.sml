(* Copyright 1989,1990,1991 by AT&T Bell Laboratories *)
(* util/arrayext.sml *)


	(*
	 * This module should be merged into the Array module...
	 *)

signature ARRAYEXT =
sig
  val listofarray : 'a array -> 'a list
  val copy : '1a array  -> '1a array
  val app : ('a -> 'b) * 'a array -> unit
  val remap : ('a -> 'a) * 'a array -> unit
  val reset : 'a array * 'a array -> unit
  val refill : 'a array * 'a -> unit
end

structure ArrayExt : ARRAYEXT =
struct

  open Array
  infix 9 sub (* Aargh! *)

  (* listofarray *)

  fun listofarray a =
      let fun loop i = (a sub i)::loop(i+1) handle Subscript => []
      in
	loop 0
      end

  (* copy *)

  fun copy a =
      if Array.length a=0 then fromList nil
      else
	 let val new = array(Array.length a,a sub 0)
	    fun loop i = (update(new,i,a sub i); loop(i+1))
	 in loop 1
	    handle Subscript => new
	 end

  (* app *)

  fun app (f,a) =  
      let fun loop i = (f(a sub i); loop(i+1))
       in loop 0 handle Subscript => ()
      end

  (* remap *)

  fun remap (f,a) =
      let fun loop i = (update(a,i,f(a sub i)); loop(i+1))
       in loop 0 handle Subscript => ()
      end

  (* reset *)

  fun reset(target,source) =
    let fun loop n = (update(target,n, source sub n); loop (n+1))
    in loop 0 handle Subscript => ()
    end

  (* refill *)

  fun refill(target,value) = 
    let fun loop n = (update(target,n,value); loop(n+1))
    in loop 0 handle Subscript => ()
    end

end

(*
 * $Log: arrayext.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:48  george
 *   Version 109.24
 *
 *)

(* bug202.1.sml *)

datatype 'a chan = CHAN of 'a

signature BADGUYS = sig
    val mkchan : unit -> '1a chan
    val ufanout : '2a chan * '2a chan list -> 'b
    val begin_str :  string -> ('a -> 'b) -> 'a -> unit
end

signature WONKY =  sig
    val ureplicas : int -> '2a chan -> '6b chan list
		(* wrong! should be int -> '2a chan -> '2a chan list *)
  end

functor buggy(bad:BADGUYS):WONKY = struct

open bad

fun ureplicas n c =     (* n unsynchronized copies of channel c *)
    let fun channels(l,0) = l
	  | channels(l,n) = channels(mkchan()::l,n-1)
	val chans = channels(nil,n)
	val _ = begin_str "replicas" ufanout(c,chans)
    in  chans
    end

end

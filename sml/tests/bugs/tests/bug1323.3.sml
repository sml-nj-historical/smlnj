(* bug1323.3.sml *)

(* Uncommented the commented code in bug1323.2.sml.  Still get
 * RegMap exception on hppa and alpha.
 *)

functor F (type code) : sig end =
struct
  fun id x = x
  val () = id()
  fun f (a,b) = id id id (a,b)
  fun g () : code  = 
	let val ((),b) = f((),g())
	 in id id b
	end
  val u = g ()
end;

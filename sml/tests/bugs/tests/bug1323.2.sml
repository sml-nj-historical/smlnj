(* bug1323.2.sml *)

(* Andrew -- 
Uncomment the commented code, and the error changes from "Gpregnum" to

  Error: Compiler bug: Contract: UsageMap on 299

which proves that the error is at CPSopt or earlier -- I strongly
suspect earlier.
*)

functor F (type code) : sig end =
struct
  fun id x = x
  val () = id()
  fun f (a,b) = id id id (a,b)
  fun g () : code  = 
	let val ((),b) = f((),g())
	 in (* id id *) b
	end
  val u = g ()
end;

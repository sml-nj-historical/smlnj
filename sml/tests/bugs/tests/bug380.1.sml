(* bug380.1.sml *)

functor Broken (S : sig
			val perform : (unit -> 'a) -> 'a
		    end) =
    struct
	val period = 0
	val name = "foo"
	val n = 0
		
	fun proc () =
	    (if n mod period = 0 then
		 S.perform (fn () => print (name ^ "\n"))
	     else
		 ();
	     proc ())
    end

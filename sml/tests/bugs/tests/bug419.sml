(* bug419.sml *)
(* requires CML *)

local open CML
in
fun placeBuffer () =
   let 
	val c = channel () 
	val b = channel () 
	val a = channel ()
	fun input_int (s:string) =
	    fold (fn(a,r) => ord(a) - ord("0") + 10 * r) (tl (rev(explode s))) 0;

	fun P1 x   = (CIO.print( "Waiting for Input on Channel a? \n");
		let
			val y =  input_int(CIO.input_line CIO.std_in)
		in
			s__3 x y
		end)
	and 
 	    s__3 x y   = ( ( send (c,y ) ; P1 y  ))

	fun P2 z   = (let val v =  accept  c in s__5 z v  end)
	and 
  	   s__5 z v  =
	( (CIO.print (" Output on Channel b!"^Integer.makestring(v)^"\n");
	P2 v  ))
   in 
	 spawn (fn () => P1 4  );
	 spawn (fn () => P2 5  );
	() 
   end
end (* local *)

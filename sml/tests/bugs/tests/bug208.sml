(* bug 208.sml *)
(* Problem: impossible error in cpsopt phase, on MIPS machine,
	 with default optimization settings
*)

functor MipsCoder(val emit : 'a -> unit)  = struct
  fun needs _ = true
  fun pass now =
  let fun gen inst =
	 if now andalso needs() then ()
	 else if now
	    then let fun gen1() = gen(raise Match)
		  in  case inst of
			NONE  => gen1()
		      | SOME b =>
			  let fun bc1f offset = ()
			      fun bc1t offset = ()
			  in  if inst=NONE then 
				   (emit((if b then bc1t else bc1f)
						inst); gen1()) 
			      else ()
			  end
		  end
	    else ()
  in  gen
  end
  val assemble  = pass true
end

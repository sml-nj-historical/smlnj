(* mk-rpc.sml
 *
 *)

structure MakeRPC =
  struct

    fun mkRPC (f : 'a -> '_b -> ('a * '_c)) = let
	  val (reqCh : ('_b * '_c CML.cond_var) CML.chan) = CML.channel ()
	(* the client side call *)
	  fun call arg = let
		val reply = CML.condVar ()
		in
		  CML.send (reqCh, (arg, reply));
		  CML.readVar reply
		end
	(* the server side entry event *)
	  fun entry state = let
		fun doCall (arg, replyV) = let
		      val (newState, result) = f state arg
		      in
			CML.writeVar(replyV, result);
			newState
		      end
		in
		  CML.wrap(CML.receive reqCh, doCall)
		end
	  in
	    {call=call, entry=entry}
	  end

    fun server entries initState = let
	  fun bind state f = f state
	  fun select state = CML.select (map (bind state) entries)
	  fun loop state = loop(select state)
	  in
	    CML.spawn (fn () => loop initState);
	    ()
	  end

  end;


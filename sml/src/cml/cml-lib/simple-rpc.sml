(* simple-rpc.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * Generators for simple RPC protocols.
 *)

structure SimpleRPC : SIMPLE_RPC =
  struct

    type 'a event = 'a CML.event

    fun call reqMB arg = let
	  val replV = SyncVar.iVar()
	  in
	    Mailbox.send(reqMb, (arg, replV));
	    SyncVar.iGet replV
	  end

    fun mkRPC f = let
	  val reqMb = Mailbox.mailbox()
	  val entryEvt = CML.wrap (
		Mailbox.recvEvt reqMb,
		fn (arg, replV) => SyncVar.iPut(replV, f arg))
	  in
	    { call = call reqMB, entryEvt = entryEvt }
	  end

    fun mkRPC_In f = let
	  val reqMb = Mailbox.mailbox()
	  val reqEvt = Mailbox.recvEvt reqMb
	  fun entryEvt state = CML.wrap (
		reqEvt,
		fn (arg, replV) => SyncVar.iPut(replV, f(arg, state)))
	  in
	    { call = call reqMB, entryEvt = entryEvt }
	  end

    fun mkRPC_Out f = let
	  val reqMb = Mailbox.mailbox()
	  val reqEvt = Mailbox.recvEvt reqMb
	  val entryEvt = CML.wrap (
		reqEvt,
		fn (arg, replV) => let val (res, state') = f arg
		  in
		    SyncVar.iPut(replV, res); state'
		  end)
	  in
	    { call = call reqMB, entryEvt = entryEvt }
	  end

    fun mkRPC_InOut f = let
	  val reqMb = Mailbox.mailbox()
	  val reqEvt = Mailbox.recvEvt reqMb
	  fun entryEvt state = CML.wrap (
		reqEvt,
		fn (arg, replV) => let val (res, state') = f(arg, state)
		  in
		    SyncVar.iPut(replV, res); state'
		  end)
	  in
	    { call = call reqMB, entryEvt = entryEvt }
	  end

  end

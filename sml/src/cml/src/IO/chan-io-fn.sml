(* chan-io-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

functor ChanIOFn (
    structure PrimIO : PRIM_IO
    structure V : MONO_VECTOR
    structure A : MONO_ARRAY
      sharing type A.array = PrimIO.array
      sharing type A.vector = V.vector = PrimIO.vector
  ) : sig

    structure PrimIO : PRIM_IO

    val mkReader : PrimIO.vector CML.chan -> PrimIO.reader
    val mkWriter : PrimIO.vector CML.chan -> PrimIO.writer

  end = struct

    structure SV = SyncVar

    structure PrimIO = PrimIO

  (* create a reader that is connected to the output port of a channel. *)
    fun mkReader ch = let
	  val closedFlg = SV.iVar()
	  val isClosedEvt = CML.wrap(SV.iGetEvt closedFlg, fn () => raise IO.ClosedStream)
	  datatype req
	    = RD of (int * unit CML.event * V.vector CML.chan)
	    | CLOSE
	  val reqCh = Mailbox.mailbox()
	  fun readVecEvt 0 = CML.alwaysEvt(V.fromList[])
	    | readVecEvt n = if (n < 0)
		then raise General.Subscript
		else CML.withNack (fn nack => let
		  val replCh = CML.channel()
		  in
		    Mailbox.send (reqCh, RD(n, nack, replCh));
		    CML.choose [
			CML.recvEvt replCh,
			isClosedEvt
		      ]
		  end)
	  fun readArrEvt {buf, i, sz} = let
		val bufLen = A.length buf
	      (* note that since readVecEvt checks for length < 0, we don't
	       * have to do those checks here.
	       *)
		val n = (case sz
		       of NONE =>
			    if (i < 0)
			      then raise General.Subscript
			      else bufLen - i
			| (SOME n) =>
			    if (i < 0) orelse (bufLen < i+n)
			      then raise General.Subscript
			      else n
		      (* end case *))
		in
		  CML.wrap (readVecEvt n, fn v => (
		    A.copyVec{dst=buf, di=i, src=v, si=0, len=NONE};
		    V.length v))
		end
	  fun close () = Mailbox.send(reqCh, CLOSE)
	  fun getData NONE = let
		val v = CML.recv ch
		in
		  if (V.length v > 0) then v else getData NONE
		end
	    | getData (SOME v) = v
	  fun server buf = (case (Mailbox.recv reqCh)
		 of RD(n, nack, replCh) => let
		      val v = getData buf
		      in
			if (V.length v > n)
			  then let
			    val v' = V.extract(v, 0, SOME n)
			    in
			      CML.select [
				  CML.wrap (nack, fn () => server(SOME v)),
				  CML.wrap (CML.sendEvt(replCh, v),
				    fn () => server(SOME(V.extract(v, n, NONE))))
				]
			    end
			  else CML.select [
			      CML.wrap (nack, fn () => server(SOME v)),
			      CML.wrap (CML.sendEvt(replCh, v), fn () => server NONE)
			    ]
		      end
		  | CLOSE => (SV.iPut(closedFlg, ()); closedServer())
		(* end case *))
	  and closedServer () = (ignore(Mailbox.recv reqCh); closedServer())
	  in
	    PrimIO.RD{
		name       = "<channel>", 
		chunkSize  = 1024,			(* ?? *)
		readVec    = CML.sync o readVecEvt,
        	readArr    = CML.sync o readArrEvt,
		readVecEvt = readVecEvt,
		readArrEvt = readArrEvt,
		avail      = fn () => NONE,		(* ?? *)
		getPos     = NONE,
		setPos     = NONE,
        	endPos     = NONE,
		verifyPos  = NONE,
		close      = close,
		ioDesc     = NONE
	      }
	  end

  (* create a writer that is connected to the input port of a channel. *)
    fun mkWriter ch = let
	  val closedFlg = SV.iVar()
	  val closedEvt = CML.wrap (SV.iGetEvt closedFlg, fn () => raise IO.ClosedStream)
	  val ch' = CML.channel()
	  fun buffer () = CML.select [
		  CML.wrap (CML.recvEvt ch', fn v => (
		    if (V.length v > 0) then CML.send(ch, v) else ();
		    buffer())),
		  closedEvt
		]
	  fun msg extract {buf, i, sz} = extract(buf, i, sz)
	  fun writeVecEvt arg = let val v = msg V.extract arg
		in
		  CML.choose [
		      closedEvt,
		      CML.wrap (CML.sendEvt (ch', v), fn () => V.length v)
		    ]
		end
	  fun writeArrEvt arg = let val v = msg A.extract arg
		in
		  CML.choose [
		      closedEvt,
		      CML.wrap (CML.sendEvt (ch', v), fn () => V.length v)
		    ]
		end
	  fun close () = SV.iPut(closedFlg, ())
	  in
	    PrimIO.WR{
		name        = "<channel>",
		chunkSize   = 1024,
		writeVec    = CML.sync o writeVecEvt,
		writeArr    = CML.sync o writeArrEvt,
		writeVecEvt = writeVecEvt,
		writeArrEvt = writeArrEvt,
		getPos      = NONE,
		setPos      = NONE,
        	endPos      = NONE,
		verifyPos   = NONE,
		close       = close,
		ioDesc      = NONE
	      }
	  end

  end;

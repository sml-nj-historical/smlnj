(* win32-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the Win32 version of the OS specific binary primitive
 * IO structure.  The Text IO version is implemented by a trivial translation
 * of these operations (see nt-text-prim-io.sml).
 *)

structure Win32BinPrimIO : OS_PRIM_IO = 
  struct

    structure SV = SyncVar

    structure PrimIO = BinPrimIO
	
    structure W32FS = Win32.FileSys
    structure W32IO = Win32.IO
    structure W32G = Win32.General

    structure V = Word8Vector
	
    type file_desc = W32G.hndl
	
    val pfi = Position.fromInt
    val pti = Position.toInt
    val pfw = Position.fromInt o W32G.Word.toInt
    val ptw = W32G.Word.fromInt o Position.toInt
	    
    val say = W32G.logMsg

    val bufferSzB = 4096

    val seek = pfw o W32IO.setFilePointer'

    fun posFns iod = 
	  if (OS.IO.kind iod = OS.IO.Kind.file)
	    then let
	      val pos : Position.int ref = ref(pfi 0)
	      fun getPos () : Position.int = !pos
	      fun setPos p = 
		    pos := seek (W32FS.IODToHndl iod, ptw p, W32IO.FILE_BEGIN)
	      fun endPos () : Position.int = (
		    case W32FS.getLowFileSize (W32FS.IODToHndl iod)
		     of SOME w => pfw w
		      | _ => raise OS.SysErr("endPos: no file size", NONE)
		    (* end case *))
	      fun verifyPos () = (
		    pos := seek (W32FS.IODToHndl iod, 0wx0, W32IO.FILE_CURRENT);
		    !pos)
	      in
		ignore (verifyPos());
		{ pos=pos,
		  getPos=SOME getPos,
		  setPos=SOME setPos,
		  endPos=SOME endPos,
		  verifyPos=SOME verifyPos
		}
	      end
	    else {
		pos=ref(pfi 0),
		getPos=NONE,setPos=NONE,endPos=NONE,verifyPos=NONE
	      }
		
    fun addCheck f (SOME g) = SOME (f g)
      | addCheck _ NONE = NONE

    fun mkReader {fd, name} = let
	  val iod = W32FS.hndlToIOD fd
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		(Syscall.doSyscall f x) before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  fun withLock' NONE = NONE
	    | withLock' (SOME f) = SOME(withLock f)
	  val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} = posFns iod
	  fun incPos k = pos := Position.+(!pos, pfi k)
	  fun blockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		f x)
	  val readEvt =
		IOManager.ioEvt(OS.IO.pollIn(Option.valOf(OS.IO.pollDesc iod)))
	  fun eventWrap f x = CML.withNack (fn nack => (
		if !closed then raise IO.ClosedStream else ();
		case (SV.mTakePoll lockMV)
		 of NONE => let
		      val replV = SV.iVar()
		      in
			CML.spawn(fn () => CML.select [
			    CML.wrap (readEvt, fn _ => SV.iPut(replV, ())),
			    nack
			  ]);
			CML.wrap(SV.iGetEvt replV, fn _ => f x)
		      end
		  | (SOME _) => CML.wrap (readEvt,
			fn _ => (SV.mPut(lockMV, ()); f x))
		(* end case *)))
	  fun readVec n = let
		val _ = CML.sync readEvt
		val v = W32IO.readVec (W32FS.IODToHndl iod,n)
		in
		  incPos (V.length v); v
		end
	  fun readArr arg = let
		val _ = CML.sync readEvt
		val k = W32IO.readArr(W32FS.IODToHndl iod,arg)
		in
		  incPos k; k
		end
	  fun close () = if !closed
		then ()
		else (closed:=true; W32IO.close (W32FS.IODToHndl iod))
	  fun avail () = if !closed
		then SOME 0
		else (case W32FS.getLowFileSize (W32FS.IODToHndl iod)
		   of SOME w => SOME (Position.-(pfw w,!pos))
		    | NONE => NONE
		  (* end case *))
	  in
	    BinPrimIO.RD{
		name		= name,
		chunkSize	= bufferSzB,
		readVec		= withLock (blockWrap readVec),
		readArr		= withLock (blockWrap readArr),
		readVecEvt	= eventWrap readVec,
		readArrEvt	= eventWrap readArr,
		avail		= withLock avail,
		getPos		= withLock' getPos,
		setPos		= withLock' setPos,
		endPos		= withLock' endPos,
		verifyPos	= withLock' verifyPos,
		close		= withLock close,
		ioDesc		= SOME iod
	      }
	  end


    val shareAll = W32G.Word.orb(W32IO.FILE_SHARE_READ, W32IO.FILE_SHARE_WRITE)

    fun checkHndl name h = if W32G.isValidHandle h
	  then h
	  else raise OS.SysErr("win32-bin-prim-io:checkHndl: "^name^": failed",NONE)

    fun openRd name = mkReader{
	    fd = checkHndl "openRd" (W32IO.createFile {
		name=name,
		access=W32IO.GENERIC_READ,
		share=shareAll,
		mode=W32IO.OPEN_EXISTING,
		attrs=0wx0
	      }),
	    name = name
	  }

    fun mkWriter {fd, name, appendMode, chunkSize} = let
	  val iod = W32FS.hndlToIOD fd
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		(Syscall.doSyscall f x) before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  fun withLock' NONE = NONE
	    | withLock' (SOME f) = SOME(withLock f)
	  val closed = ref false
	  fun ensureOpen () = if !closed then raise IO.ClosedStream else ()
	  fun putV x = W32IO.writeVec x
	  fun putA x = W32IO.writeArr x
	  fun write put arg = (ensureOpen(); put(W32FS.IODToHndl iod, arg))
	  val writeEvt =
		IOManager.ioEvt(OS.IO.pollOut(Option.valOf(OS.IO.pollDesc iod)))
	  fun eventWrap f x = CML.withNack (fn nack => (
		if !closed then raise IO.ClosedStream else ();
		case (SV.mTakePoll lockMV)
		 of NONE => let
		      val replV = SV.iVar()
		      in
			CML.spawn(fn () => CML.select [
			    CML.wrap (writeEvt, fn _ => SV.iPut(replV, ())),
			    nack
			  ]);
			CML.wrap(SV.iGetEvt replV, fn _ => f x)
		      end
		  | (SOME _) => CML.wrap (writeEvt,
			fn _ => (SV.mPut(lockMV, ()); f x))
		(* end case *)))
	  fun close () = if !closed
		then ()
		else (closed:=true; W32IO.close (W32FS.IODToHndl iod))
          val {pos, getPos, setPos, endPos, verifyPos} = posFns (iod)
	  in
	    BinPrimIO.WR{
		name		= name,
		chunkSize	= chunkSize,
		writeVec	= withLock (write putV),
		writeArr	= withLock (write putA),
		writeVecEvt	= eventWrap (write putV),
		writeArrEvt	= eventWrap (write putA),
		getPos		= withLock' getPos,
		setPos		= withLock' setPos,
		endPos		= withLock' endPos,
		verifyPos	= withLock' verifyPos,
		close		= withLock close,
		ioDesc		= SOME iod
	      }
	  end

    fun openWr name = mkWriter{
	    fd = checkHndl "openWr" (W32IO.createFile{
		name=name,
		access=W32IO.GENERIC_WRITE,
		share=shareAll,
		mode=W32IO.CREATE_ALWAYS,
		attrs=W32FS.FILE_ATTRIBUTE_NORMAL
	      }),
	    name = name,
	    appendMode = false,
	    chunkSize = bufferSzB
	  }

    fun openApp name = let
	  val h = checkHndl "openApp" (W32IO.createFile {
		  name=name,
		  access=W32IO.GENERIC_WRITE,
		  share=shareAll,
		  mode=W32IO.OPEN_EXISTING,
		  attrs=W32FS.FILE_ATTRIBUTE_NORMAL
		})
	  val _ = W32IO.setFilePointer' (h,0wx0,W32IO.FILE_END)
	  in
	    mkWriter{fd = h, name = name, appendMode = true, chunkSize = bufferSzB}
	  end

  end; (* Win32BinPrimIO *)


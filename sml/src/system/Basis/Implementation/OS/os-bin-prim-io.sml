(* os-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the OS generic version of the binary primitive
 * IO structure.  The Text IO version is implemented by a trivial translation
 * of these operations (see os-text-prim-io.sml).
 *
 *)

local
    structure Position = PositionImp
    structure OS = OSImp
in	
structure OSBinPrimIO : OS_BIN_PRIM_IO = 
  struct

    structure PrimIO = BinPrimIO

    structure Vec = Word8Vector
(*
    structure PF = Posix.FileSys
    structure PIO = Posix.IO
*)

    datatype file_desc = FD of SMLBasis.IODesc_t
    fun mkFileDesc iod = FD iod
    fun unFileDesc (FD iod) = iod
    fun toIodesc(iod: SMLBasis.IODesc_t): OS.IO.iodesc = ????

(*
    type file_desc = PF.file_desc  (* Unix *)
     (* where PF=Posix.FileSys (posix-filesys.sml):
          datatype file_desc = FD of {fd : SysInt.int} *)
    type file_desc = W32G.hndl     (* Win32 *)
     (* where W32G=Win32_General (win32/win32-general.sml):
          type hndl = Word32Imp.word *)

    what is relation between file_desc and SMLBasis.IODesc_t (= pointer)? 
*)

    fun announce s x y = (
	  (*print "Posix: "; print (s:string); print "\n"; *)
	  x y)

    val bufferSzB = 4096

    val pfi = Position.fromInt
    val pti = Position.toInt
    val pti32 = Position.toLarge   (* Large = Int32 ?! *)
    val pfi32 = Position.fromLarge (* Large = Int32 ?! *)

    fun isRegFile (iod: SMLBasis.IODesc_t) = (* generic *)
        SMLBasis.ioDescKind iod = IOD_KIND_FILE

(*
    fun isRegFile (fd: file_desc) = (* Unix *)
        PF.ST.isReg(PF.fstat fd) 
    fun isRegFile (fd: file_desc) = (* Win32 *)
        OS.IO.kind (W32FS.hndlToIOD fd) = OS.IO.Kind.file
*)

    fun posFns (closed, iod) =
        if (isRegFile iod)
	then let
	    val pos = ref(Position.fromInt 0)
	    fun getPos () = !pos
	    fun setPos p = (
		  if !closed then raise IO.ClosedStream else ();
		  pos := announce "seek" SMLBasis.seek(iod,pti32 p))
	    fun endPos () = (
		  if !closed then raise IO.ClosedStream else ();
		  pfi32(SMLBasis.fileSize iod))
	    fun verifyPos () =
		(pos := pfi32(SMLBasis.seekCur(iod)); !pos)
	    in
	      ignore (verifyPos());
	      { pos = pos,
		getPos = SOME getPos,
		setPos = SOME setPos,
		endPos = SOME endPos,
		verifyPos = SOME verifyPos
	      }
	    end
	else {pos = ref(Position.fromInt 0),
	      getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE
	    }

    fun mkReader {fd as FD(iod), name, initBlkMode} =
        if not SMLBasis.nonBlockingSupported andalso not initBlkMode
          then raise IO.NonblockingNotSupported else
        let val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, iod)
          val blocking = ref initBlkMode
          fun blockingOn () = (SMLBasis.blockingOn(iod); blocking := true)
	  fun blockingOff () = (SMLBasis.blockingOff(iod); blocking := false)
	  fun incPos k = pos := Position.+(!pos, pfi k)
	  fun readVec n = let
		val v = announce "read" SMLBasis.readVec(iod, Int32.fromInt n)
		in
		  incPos (Vec.length v); v
		end
	  fun readArr arg = let
		val k = announce "readBuf" SMLBasis.readArr(iod, Int32.fromInt arg)
		in
		  incPos k; k
		end
	  fun blockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		if !blocking then () else blockingOn();
		f x)
	  fun noBlockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		if !blocking then blockingOff() else ();
		((* try *) SOME(f x)
		  handle (e as OS.SysErr(_, SOME cause)) =>
                     if cause = Posix.Error.again then NONE else raise e
		(* end try *)))
	  fun close () = if !closed
		then ()
		else (closed:=true; announce "close" SMLBasis.closeFile iod)
	  fun avail () = if !closed
		  then SOME 0
		else if isRegFile iod
		  then SOME(Position.-(pfi32(SMLBasis.iodescSize(iod)), !pos))
		  else NONE
	  in
	    BinPrimIO.RD{
		name		= name,
		chunkSize	= bufferSzB,
		readVec		= SOME(blockWrap readVec),
		readArr		= SOME(blockWrap readArr),
		readVecNB	= SOME(noBlockWrap readVec),
		readArrNB	= SOME(noBlockWrap readArr),
		block		= NONE,
		canInput	= NONE,
		avail		= avail,
		getPos		= getPos,
		setPos		= setPos,
		endPos		= endPos,
		verifyPos	= verifyPos,
		close		= close,
		ioDesc		= SOME(toIodesc(iod))
	      }
	  end

	     
    fun openRd name = mkReader{
	    fd = FD(announce "openf" SMLBasis.openFile(name,0(*???*))),
	    name = name,
	    initBlkMode = true
	  }


    (* dbm: do we need addCheck from Win32BinPrimIO ? *)

    fun mkWriter {fd as FD(iod), name, initBlkMode, appendMode, chunkSize} =
        if not SMLBasis.nonBlockingSupported andalso not initBlkMode
          then raise IO.NonblockingNotSupported else
	let val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, iod)
	  fun incPos k = (pos := Position.+(!pos, pfi k); k)
	  val blocking = ref initBlkMode
	  fun updateStatus() = SMLBasis.updateStatus(iod,appendMode,!blocking)
	  fun ensureOpen () = if !closed then raise IO.ClosedStream else ()
	  fun ensureBlock (x) =
		if !blocking = x then () else (blocking := x; updateStatus())
	  fun putV (iod,buf,i,sz) =
	      incPos(announce "writeVec" SMLBasis.writeBinVec(iod,buf,i,sz))
	  fun putA (fd,buf,i,sz) =
              incPos(announce "writeArr" SMLBasis.writeBinArr(iod,buf,i,sz))
	  fun write (put, block) {buf,i,sz} = (
		ensureOpen(); ensureBlock block; 
		put(fd,buf,i,case sz of NONE => ? | SOME n => Int32.fromInt n))
	  fun handleBlock writer arg = SOME(writer arg)
		handle (e as OS.SysErr(_, SOME cause)) => 
 		  if cause = Posix.Error.again then NONE else raise e
	  fun close () = if !closed
		then ()
		else (closed:=true; announce "close" PIO.close fd)
	  in
	    BinPrimIO.WR{
		name		= name,
		chunkSize	= chunkSize,
		writeVec	= SOME(write(putV,true)),
		writeArr	= SOME(write(putA,true)),
		writeVecNB	= if SMLBasis.nonBlockingSupported
			          then SOME(handleBlock(write(putV,false)))
				  else NONE,
		writeArrNB	= if SMLBasis.nonBlockingSupported
			          then SOME(handleBlock(write(putA,false)))
				  else NONE,
		block		= NONE,
		canOutput	= NONE,
		getPos		= getPos,
		setPos		= setPos,
		endPos		= endPos,
		verifyPos	= verifyPos,
		ioDesc		= SOME(toIodesc iod),
		close		= close
	      }
	  end

(* fold this into createFile
    val standardMode = PF.S.flags[	(* mode 0666 *)
	    PF.S.irusr, PF.S.iwusr,
	    PF.S.irgrp, PF.S.iwgrp,
	    PF.S.iroth, PF.S.iwoth
	  ]
*)

    fun openWr name = mkWriter{
	    fd = FD(SMLBasis.createFile(name, false)),
	    name = name,
	    initBlkMode = true,
	    appendMode = false,
	    chunkSize = bufferSzB
	  }

    fun openApp name = mkWriter{
	    fd = FD(SMLBasis.createFile(name, true)),
	    name = name,
	    initBlkMode	= true,
	    appendMode = true,
	    chunkSize = bufferSzB
	  }

  end (* PosixBinPrimIO *)
end


(* 

SMLBasis.seek:
Unix: PIO.lseek(fd,p,PIO.SEEK_SET)
Win32: let val seek = pfw o W32IO.setFilePointer'
       in seek (W32FS.IODToHndl iod, ptw p, W32IO.FILE_BEGIN) end

SMLBasis.seekCur:
Unix: PIO.lseek(fd, pfi 0, PIO.SEEK_CUR)
Win32: let val seek = pfw o W32IO.setFilePointer'
       in seek (W32FS.IODToHndl iod, 0wx0, W32IO.FILE_CURRENT) end

SMLBasis.fileSize:
Unix: PF.ST.size(PF.fstat fd)
Windows: W32FS.getLowFileSize (W32FS.IODToHndl iod)

SMLBasis.blockingOn:
Unix: PIO.setfl(fd, PIO.O.flags[])
Win32: not supported

SMLBasis.blockingOff:
Unix: PIO.setfl(fd, PIO.O.nonblock)
Win32: not supported

SMLBasis.openFile(name,?):
Unix: PF.openf(name,PIO.O_RDONLY,PF.O.flags[])
Win32:
  let fun checkHndl name h = 
	if W32G.isValidHandle h then h
	else raise OS.SysErr ("win32-bin-prim-io:checkHndl: "^name^": failed",NONE)
   in checkHndl "openRd" (W32IO.createFile{name=name,
                                           access=W32IO.GENERIC_READ,
					   share=shareAll,
					   mode=W32IO.OPEN_EXISTING,
					   attrs=0wx0})
  end


SMLBasis.updateStatus(iod, appendMode: bool,blocking: bool): unit:
Unix: let val appendFS = PIO.O.flags(if appendMode then [PIO.O.append] else nil)
          val flgs = if blocking then appendFS
		      else PIO.O.flags[PIO.O.nonblock, appendFS]
       in PIO.setfl(fd, flgs)
      end
Win32: does nothing?

SMLBasis.createFile (append: bool):
Unix:
  let val standardMode = PF.S.flags[	(* mode 0666 *)
	    PF.S.irusr, PF.S.iwusr,
	    PF.S.irgrp, PF.S.iwgrp,
	    PF.S.iroth, PF.S.iwoth
	  ]
      fun createFile (name, mode, flags) =
	   PF.createf(name, mode, flags, standardMode)
   in if append then createFile(name, PIO.O_WRONLY, PF.O.append)
      else createFile(name, PIO.O_WRONLY, PF.O.trunc)
  end
Win32:
  let val mode = if append then W32IO.OPEN_EXISTING else W32IO.CREATE_ALWAYS
      fun checkHndl name h = 
	if W32G.isValidHandle h then h
	else raise OS.SysErr ("win32-bin-prim-io:checkHndl: "^name^": failed",NONE)
   in checkHndl "openWr"
       (W32IO.createFile{name=name,
			 access=W32IO.GENERIC_WRITE,
			 share=shareAll,
			 mode=W32IO.CREATE_ALWAYS,
			 attrs=W32FS.FILE_ATTRIBUTE_NORMAL
			 })
  end

*)

(* os-filesys.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The Posix implementation of the generic file system interface.
 *
 *)

local
    structure SysWord = SysWordImp
    structure Word = WordImp
in
structure OS_FileSys : OS_FILE_SYS =
  struct

(*  structure P_FSys = Posix.FileSys *)

    val sysWordToWord = Word.fromLargeWord o SysWord.toLargeWord

    (* should be finalized *)
    datatype dirstream = DIRSTREAM of SMLBasis.Directory_t
    fun mkDirstream d = DIRSTREAM d
    fun unDirstream (DIRSTREAM d) = d

    val openDir   = mkDirstream o SMLBasis.openDir
    val readDir   = SMLBasis.readDir o unDirstream
    val rewindDir = SMLBasis.rewindDir o unDirstream
    val closeDir  = SMLBasis.closedDir o unDirstream

    val chDir  = SMLBasis.chdir
    val getDir = SMLBasis.getDir
(*    local
      structure S = P_FSys.S
      val mode777 = S.flags[S.irwxu, S.irwxg, S.irwxo]
    in
*)
    fun mkDir path = SMLBasis.mkdir(path) (* , mode777) *)
(*    end *)
    val rmDir  = SMLBasis.rmdir
    val isDir  = SMLBasis.isDir

    val isLink   = SMLBasis.isLink
    val readLink = SMLBasis.readlink

  (* the maximum number of links allowed *)
    val maxLinks = 64

    structure P = OS_Path  (* dbm: which OS_Path? since OS_Path is
			    * not generic *)

    val fullPath = SMLBasis.fullPath

    val realPath = SMLBasis.realPath

    (* dbm: are these functions the most efficient way to convert between
     * Time.t (whose * internal representation is TIME{sec:int,usec:int})
     * and Time_t = {sec: Int32.int, usec: Int32.int}? *)
    fun toTime({sec,usec}: SMLBasis.Time_t): SMLBasis.Time_t =
        Time.+(Time.fromSeconds(Int32.toInt sec),
	       Time.fromMicroseconds(Int32.toInt usec))

    fun fromTime(t: Time.t): SMLBasis.Time_t =
	let val sec = Time.toSeconds t
	    val usec = Time.toMicroseconds t - 1000000 * sec
	 in {sec = Int32.fromInt sec, usec = Int32.fromInt usec}
	end

    val fileSize =  (* dbm: missing from SMLBasis *)
        Position.fromLarge o SMLBasis.fileSize  (* fromLarge is from Int32.int !? *)

    val modTime  = toTime o SMLBasis.modTime   (* dbm: missing from SMLBasis *)

    fun setTime(name,time_op) = 
        let val t_op = Option.map fromTime time_op
	 in SMLBasis.setTime(name, t_op)
	end

    val remove   = SMLBasis.unlink
    val rename   = SMLBasis.rename

    structure A : sig
	datatype access_mode = A_READ | A_WRITE | A_EXEC
      end = Posix.FileSys
    open A

    fun access (path, al) = let
	  fun cvt A_READ = SMLBasis.A_READ
	    | cvt A_WRITE = SMLBasis.A_WRITE
	    | cvt A_EXEC = SMLBasis.A_EXEC
	  in
	    SMLBasis.access (path, List.map cvt al)  
           (* dbm: type problem - SMLBasis.access takes Int32.int as second
	    * argument *)
	  end

    val tmpName : unit -> string = SMLBasis.tmpName

(*    datatype file_id = FID of {dev : SysWord.word, ino : SysWord.word} *)
    datatype file_id = FID of Word8Vector.vector

    (* dbm: how to unpack the Word8Vector returned by SMLBasis.fileId *)
    fun fileId fname = FID (SMLBasis.fileId fname)

    fun hash (FID v) = (* hash on Word8Vector.vector *)
     sysWordToWord(
	  SysWord.+(SysWord.<<(dev, 0w16), ino))

    fun compare (FID v1, FID v2) = Word8Vector.collate Word8.compare (v1,v2)
      (* from new Basis!!! *)

  end
end

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
    datatype dirstream = DIRSTREAM of SMLBasis.ML_directory_t
    fun mkDirstream d = DIRSTREAM d
    fun unDirstream (DIRSTREAM d) = d

    val openDir   = mkDirstream o SMLBasis.openDir
    val readDir   = SMLBasis.readDir o unDirstream
    val rewindDir = SMLBasis.rewindDir o unDirstream
    val closeDir  = SMLBasis.closeDir o unDirstream

    val chDir  = SMLBasis.chDir
    val getDir = SMLBasis.getDir
(*    local
      structure S = P_FSys.S
      val mode777 = S.flags[S.irwxu, S.irwxg, S.irwxo]
    in
*)
    fun mkDir path = SMLBasis.mkDir path (* , mode777) *)
(*    end *)
    val rmDir  = SMLBasis.rmDir
    val isDir  = SMLBasis.isDir

    val isLink   = SMLBasis.isLink
    val readLink = SMLBasis.readLink

  (* the maximum number of links allowed *)
    val maxLinks = 64

    structure P = OS_Path  (* dbm: which OS_Path? since OS_Path is
			    * not generic *)

    fun fullPath _ = raise Fail "fullPath not yet implemented"

    fun realPath _ = raise Fail "realPath not yet implemented"

    val fileSize =  (* dbm: missing from SMLBasis *)
	(* fromLarge is from Int32.int !? *)
        PositionImp.fromLarge o SMLBasis.fileSize

    fun modTime f =
	Time.TIME { seconds = SMLBasis.modTime f,
		    uSeconds = 0 }

    fun setTime(name, time_op) = 
	SMLBasis.setTime(name, Option.map (fn Time.TIME t => t) time_op)

    val remove   = SMLBasis.removeFile
    fun rename { old, new } = SMLBasis.renameFile (old, new)

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    fun access (path, al) = let
	  fun cvt A_READ = SMLBasis.A_READ
	    | cvt A_WRITE = SMLBasis.A_WRITE
	    | cvt A_EXEC = SMLBasis.A_EXEC
	  fun join (a, m) = cvt a + m
    in
	SMLBasis.fileAccess (path, foldl join 0 al)
    end

    val tmpName : unit -> string = SMLBasis.tmpName

(*    datatype file_id = FID of {dev : SysWord.word, ino : SysWord.word} *)
    datatype file_id = FID of Word8Vector.vector

    (* dbm: how to unpack the Word8Vector returned by SMLBasis.fileId *)
    fun fileId fname = FID (SMLBasis.fileId fname)

    fun hash (FID v) = (* hash on Word8Vector.vector *)
	(*
	sysWordToWord(
		      SysWord.+(SysWord.<<(dev, 0w16), ino))
	*)
	raise Fail "hash not yet implemented"

    fun compare (FID v1, FID v2) = Word8Vector.collate Word8Imp.compare (v1,v2)
      (* from new Basis!!! *)

  end
end

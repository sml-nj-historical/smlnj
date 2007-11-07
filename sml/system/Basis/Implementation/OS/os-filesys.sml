(* os-filesys.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 *
 * A generic implementation of OS.FileSys
 *
 *)

local
    structure SysWord = SysWordImp
    structure Word = WordImp
in
structure OS_FileSys : OS_FILE_SYS =
  struct

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
    val  mkDir = SMLBasis.mkDir
    val rmDir  = SMLBasis.rmDir
    val isDir  = SMLBasis.isDir

    val isLink   = SMLBasis.isLink
    val readLink = SMLBasis.readLink

  (* the maximum number of links allowed *)
    val maxLinks = 64

    structure P = OS_Path  (* dbm: which OS_Path? since OS_Path is
			    * not generic *)

  (* A generic(?) implementation of fullPath *)
    fun fullPath p = let
	  val oldCWD = getDir()
	  fun mkPath (vol, pathFromRoot) =
		P.toString{isAbs=true, vol=vol, arcs=List.rev pathFromRoot}
	  fun walkPath (0, _, _, _) =
	        raise Assembly.SysErr("too many links", NONE)
	    | walkPath (n, vol, pathFromRoot, []) =
		mkPath (vol, pathFromRoot)
	    | walkPath (n, vol, pathFromRoot, ""::al) =
		walkPath (n, vol, pathFromRoot, al)
	    | walkPath (n, vol, pathFromRoot, a::al) =
	        if a = P.currentArc then
		    walkPath (n, vol, pathFromRoot, al)
		else if a = P.parentArc then
		    case pathFromRoot of
			[] => walkPath (n, vol, [], al)
		      | _::r => (chDir P.parentArc; walkPath (n, vol, r, al))
		else if isLink a then
		    expandLink (n, vol, pathFromRoot, a, al)
		else if List.null al then
		    mkPath (vol, a :: pathFromRoot)
		else
		    (chDir a; walkPath (n, vol, a :: pathFromRoot, al))
	  and expandLink (n, vol, pathFromRoot, link, rest) = (
		case P.fromString (readLink link)
		 of {isAbs=false, arcs, ...} =>
		      walkPath (n-1, vol, pathFromRoot, arcs @ rest)
		  | {isAbs=true, vol, arcs, ...} =>
		      gotoRoot (n-1, vol, arcs @ rest)
		(* end case *))
	  and gotoRoot (n, vol, arcs) = (
		chDir (P.toString { isAbs = true, arcs = [], vol = vol });
		walkPath (n, vol, [], arcs))
	  fun computeFullPath (vol, arcs) =
		(gotoRoot(maxLinks, vol, arcs) before chDir oldCWD)
		  handle ex => (chDir oldCWD; raise ex)
	  in
	    case (P.fromString p)
	     of {isAbs=false, arcs, ...} => let
		  val {arcs=arcs', vol, ...} = P.fromString(oldCWD)
		  in
		    computeFullPath (vol ,arcs' @ arcs)
		  end
	      | {isAbs=true, vol, arcs, ...} => computeFullPath (vol, arcs)
	    (* end case *)
	  end

    fun realPath p =
	if P.isAbsolute p then fullPath p
	else P.mkRelative { path = fullPath p,
			    relativeTo = fullPath (getDir ()) }

    val fileSize =
        PositionImp.fromLarge o Int32Imp.toLarge o SMLBasis.fileSize

    val modTime = TimeImp.fromSeconds o Int32Imp.toLarge o SMLBasis.modTime

    fun setTime(name, time_op) = 
	SMLBasis.setTime(name, Option.map TimeImp.toTime_t time_op)

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

    fun hash (FID v) = let
	(* FIXME: does this give a reasonably distribution (see
	 * implementation note in Basis spec)? *)
	infix ## << ||
	val op ## =
	    SysWord.fromLargeWord o Word8Imp.toLargeWord o Word8Vector.sub
	val op << = SysWord.<<
	val op || = SysWord.orb
	val n = Word8Vector.length v
	fun isEven i = WordImp.andb (WordImp.fromInt i, 0w1) = 0w0
	(* e gets byte i from the vector and then puts it
	 *   - into byte 0 and 2 of the result for even i
	 *   - into byte 1 and 3 of the result for odd i *)
	fun e i = let
	    val x = v##i
	    val y = x || (x << 0w16)
	in
	    if isEven i then y else y << 0w8
	end
	fun sum (w, i) = if i >= n then w else sum (w + e i, i + 1)
    in
	sysWordToWord (sum (0w0, 0))
    end

    fun compare (FID v1, FID v2) = Word8Vector.collate Word8Imp.compare (v1,v2)
      (* from new Basis!!! *)

  end
end

(* os-filesys.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The implementation of the generic file system interface (OS.FileSys).
 * The OS dependencies are mostly hidden in the run-time system support,
 * but the implementation of fullPath and realPath are UNIX specific.
 *
 *)

structure OS_FileSys : OS_FILE_SYS =
  struct

    type c_dirstream = Core.Assembly.object	(* the underlying C DIRSTREAM *)

    datatype dirstream = DS of {
	dirStrm : c_dirstream,
	isOpen : bool ref
      }

    fun osFunc x = CInterface.c_function "SMLNJ-OS" x

    local
      val openDir' : string -> c_dirstream	= osFunc "openDir"
      val readDir' : c_dirstream -> string	= osFunc "readDir"
      val rewindDir' : c_dirstream -> unit	= osFunc "rewindDir"
      val closeDir' : c_dirstream -> unit	= osFunc "closeDir"
    in
    fun openDir path = DS{
	    dirStrm = openDir' path,
	    isOpen = ref true
	  }
    fun readDir (DS{dirStrm, isOpen = ref false}) =
	  PreBasis.error "readDir on closed directory stream"
      | readDir (DS{dirStrm, ...}) = readDir' dirStrm
    fun rewindDir (DS{dirStrm, isOpen = ref false}) =
	  PreBasis.error "rewindDir on closed directory stream"
      | rewindDir (DS{dirStrm, ...}) = rewindDir' dirStrm
    fun closeDir (DS{dirStrm, isOpen = ref false}) = ()
      | closeDir (DS{dirStrm, isOpen}) = (
	  isOpen := false;
	  closeDir' dirStrm)
    end (* local *)

    val chDir  : string -> unit		= osFunc "chDir"
    val getDir : unit -> string		= osFunc "getDir"
    val mkDir  : string -> unit		= osFunc "mkDir"
    val rmDir  : string -> unit		= osFunc "removeDir"
    val isDir  : string -> bool		= osFunc "isDir"

    val isLink   : string -> bool	= osFunc "isLink"
    val readLink : string -> string	= osFunc "readLink"

  (* the maximum number of links allowed *)
    val maxLinks = 64

    structure P = OS_Path;

  (* A UNIX specific implementation of fullPath *)
    fun fullPath p = let
	  val oldCWD = getDir()
	  fun mkPath pathFromRoot =
		P.toString{isAbs=true, vol="", arcs=List.rev pathFromRoot}
	  fun walkPath (0, _, _) = raise PreBasis.SysErr("too many links", NONE)
	    | walkPath (n, pathFromRoot, []) =
		mkPath pathFromRoot
	    | walkPath (n, pathFromRoot, ""::al) =
		walkPath (n, pathFromRoot, al)
	    | walkPath (n, pathFromRoot, "."::al) =
		walkPath (n, pathFromRoot, al)
	    | walkPath (n, [], ".."::al) =
		walkPath (n, [], al)
	    | walkPath (n, _::r, ".."::al) = (
		chDir ".."; walkPath (n, r, al))
	    | walkPath (n, pathFromRoot, [arc]) =
		if (isLink arc)
		  then expandLink (n, pathFromRoot, arc, [])
		  else mkPath (arc::pathFromRoot)
	    | walkPath (n, pathFromRoot, arc::al) =
		if (isLink arc)
		  then expandLink (n, pathFromRoot, arc, al)
		  else (chDir arc; walkPath (n, arc::pathFromRoot, al))
	  and expandLink (n, pathFromRoot, link, rest) = (
		case (P.fromString(readLink link))
		 of {isAbs=false, arcs, ...} =>
		      walkPath (n-1, pathFromRoot, List.@(arcs, rest))
		  | {isAbs=true, arcs, ...} =>
		      gotoRoot (n-1, List.@(arcs, rest))
		(* end case *))
	  and gotoRoot (n, arcs) = (
		chDir "/";
		walkPath (n, [], arcs))
	  fun computeFullPath arcs =
		(gotoRoot(maxLinks, arcs) before chDir oldCWD)
		  handle ex => (chDir oldCWD; raise ex)
	  in
	    case (P.fromString p)
	     of {isAbs=false, arcs, ...} => let
		  val {arcs=arcs', ...} = P.fromString(oldCWD)
		  in
		    computeFullPath (List.@(arcs', arcs))
		  end
	      | {isAbs=true, arcs, ...} => computeFullPath arcs
	    (* end case *)
	  end

    fun realPath p = if (P.isAbsolute p)
	  then fullPath p
	  else P.mkRelative (fullPath p, fullPath(getDir()))

    local
      val modTime' : string -> int			= osFunc "modTime"
      val setTime' : (string * int option) -> unit	= osFunc "setTime"
      val rename'  : (string * string) -> unit		= osFunc "rename"
    in
    fun modTime path = let val s = modTime' path
	  in
	    PreBasis.TIME{sec=s, usec=0}
	  end
    fun setTime (path, SOME(PreBasis.TIME{sec, usec})) = setTime' (path, SOME sec)
      | setTime (path, NONE) = setTime' (path, NONE)
    val remove : string -> unit	= osFunc "remove"
    fun rename {old, new} = rename'(old, new)
    end (* local *)

    datatype access = A_READ | A_WRITE | A_EXEC

    local
      val access' : (string * int list) -> bool = osFunc "access"
      val map_mode = List.map (fn A_READ => 0 | A_WRITE => 1 | A_EXEC => 2)
    in
    fun access (path, alist) = access' (path, map_mode alist)
    end (* local *)

    fun tmpName {dir : string option, prefix : string option} =
	  raise Fail "OS.FileSys.tmpName unimplemented"

  end; (* FILE_SYS *)


(*
 * $Log: os-filesys.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:26  george
 *   Version 109.24
 *
 *)

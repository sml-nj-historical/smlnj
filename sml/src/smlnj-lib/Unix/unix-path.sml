(* unix-path.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure UnixPath : UNIX_PATH =
  struct

    datatype access_mode = datatype OS.FileSys.access_mode

(** WHAT IS THIS IN POSIX??? **)
    datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK


  (** Path lists **)

    datatype path_list = PathList of string list

    exception NoSuchFile

    fun getPath () = let
	  val path = (case (UnixEnv.getEnv "PATH") of (SOME p) => p | _ => "")
	  in
	    PathList(String.fields (fn #":" => true | _ => false) path)
	  end (* getPath *)

    local
      structure ST = Posix.FileSys.ST
      fun isFileTy (path, ty) = let
	    val st = Posix.FileSys.stat path
	    in
	      case ty
	       of F_REGULAR => ST.isReg st
		| F_DIR => ST.isDir st
		| F_SYMLINK => ST.isLink st
		| F_SOCK => ST.isSock st
		| F_CHR => ST.isChr st
		| F_BLK => ST.isBlk st
	      (* end case *)
	    end
      fun access mode pathname = (OS.FileSys.access(pathname, mode))
      fun accessAndType (mode, ftype) pathname = (
	    OS.FileSys.access(pathname, mode)
	    andalso isFileTy(pathname, ftype))
	      handle _ => false
    (* return the first path p in the pathlist, such that p/name satisfies
     * the predicate.
     *)
      fun findFile' (PathList l, pred) fname = let
	    fun find [] = raise NoSuchFile
	      | find (p::r) = let val pn = OS.Path.joinDirFile{dir=p, file=fname}
		  in
		    if (pred pn) then pn else find r
		  end
	    in
	      if (OS.Path.isAbsolute fname)
	        then if (pred fname) then fname else raise NoSuchFile
	        else find l
	    end
    (* return the list of paths p in the pathlist, such that p/name satisfies
     * the predicate.
     *)
      fun findFiles' (PathList l, pred) fname = let
	    fun find ([], l) = rev l
	      | find (p::r, l) = let val pn = OS.Path.joinDirFile{dir=p, file=fname}
		  in
		    if (pred pn) then find (r, pn::l) else find (r, l)
		  end
	    in
	      if (OS.Path.isAbsolute fname)
                then if (pred fname) then [fname] else []
                else find (l, [])
	    end
    in
    fun findFile (pl, mode) = findFile' (pl, access mode)
    fun findFiles (pl, mode) = findFiles' (pl, access mode)
    fun findFileOfType (pl, ftype, mode) =
	  findFile' (pl, accessAndType(mode, ftype))
    fun findFilesOfType (pl, ftype, mode) =
	  findFiles' (pl, accessAndType(mode, ftype))
    end (* local *)

  end (* UnixPath *)

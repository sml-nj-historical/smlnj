(* unix-path-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * AUTHOR:  John Reppy and Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *	    erg@research.att.com
 *)

signature UNIX_PATH =
  sig

    type path_list

    val getPath : unit -> path_list
	(* get the user's PATH environment variable. *)

    datatype access_mode = A_READ | A_WRITE | A_EXEC
      sharing type OS.FileSys.access_mode = access_mode
    datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK
(** what is the type in POSIX??? **)

    exception NoSuchFile

    val findFile : (path_list * access_mode list) -> string -> string
    val findFiles : (path_list * access_mode list) -> string -> string list

    val findFileOfType : (path_list * file_type * access_mode list) -> string -> string
    val findFilesOfType : (path_list * file_type * access_mode list) -> string -> string list

  end (* UNIX_PATH *)

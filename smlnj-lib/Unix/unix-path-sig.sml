(* unix-path-sig.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *)

signature UNIX_PATH =
  sig

    type path_list = string list

    val getPath : unit -> path_list
	(* get the user's PATH environment variable. *)

    datatype access_mode = datatype OS.FileSys.access_mode
    datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK
(** what is the type in POSIX??? **)

    exception NoSuchFile

  (* findFile (paths, mode) name
   * returns the p/name, where p is the first path in paths such that p/name
   * has the given access modes.
   *)
    val findFile : (path_list * access_mode list) -> string -> string

  (* findFiles (paths, mode) name
   * returns a list of p/name, where p is in paths and p/name has the given access modes.
   *)
    val findFiles : (path_list * access_mode list) -> string -> string list

  (* findFile (paths, mode) name
   * returns the p/name, where p is the first path in paths such that p/name
   * has the given file type.
   *)
    val findFileOfType : (path_list * file_type * access_mode list) -> string -> string

  (* findFiles (paths, mode) name
   * returns a list of p/name, where p is in paths and p/name has the given file type.
   *)
    val findFilesOfType : (path_list * file_type * access_mode list) -> string -> string list

  end (* UNIX_PATH *)

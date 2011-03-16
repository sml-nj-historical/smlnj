(* path-util-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Various higher-level pathname and searching utilities.
 *)

signature PATH_UTIL =
  sig

  (* findFile paths name
   * returns SOME(p/name), where p is the first path in paths such that p/name exists.
   * If no such file exists, then NONE is returned.
   *)
    val findFile : string list -> string -> string option

  (* findFiles (paths, mode) name
   * returns a list of p/name values, where p is in paths and p/name exists.
   *)
    val findFiles : string list -> string -> string list

  (* existsFile pred paths name
   * returns SOME(p/name), where p is the first path in paths such that p/name satisfies
   * the given predicate.  If no such file exists, then NONE is returned.
   *)
    val existsFile : (string -> bool) -> string list -> string -> string option

  (* allFiles pred paths name
   * returns a list of all p/name values, such that p is in paths and p/name satisfies
   * the given predicate.  The order of the path list is preserved in the result.
   *)
    val allFiles : (string -> bool) -> string list -> string -> string list

  (* findExe paths name
   * returns SOME(p/name), where p is the first path in paths such that p/name exists and
   * is executable.  If no such file exists, then NONE is returned.
   *)
    val findExe : string list -> string -> string option

  end

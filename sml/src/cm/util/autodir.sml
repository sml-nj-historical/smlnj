(*
 * Opening files for output while automagically creating any
 * necessary directories.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure AutoDir :> sig
    val openBinOut : string -> BinIO.outstream
    val openTextOut : string -> TextIO.outstream
end = struct

    structure P = OS.Path
    structure F = OS.FileSys

    fun fileExists n = F.access (n, []) handle _ => false

    fun openOut fileopener p = let
	fun mkDir d =
	    F.mkDir d handle exn => (if fileExists d then () else raise exn)
	fun generic (maker, pmaker, p) =
	    maker p
	    handle exn => let
		val dir = P.dir p
	    in
		(* If the parent dir exists, then we must consider
		 * these cases:
		 *   - non-parallel: we should signal an error
		 *   - parallel: somebody else may have made this dir
		 *      in the meantime, so we should try again
		 * Both cases can be handled by simply calling maker
		 * again.  (It will fail in the non-parallel case, but
		 * that's actually what we want.) *)
		if dir = "" orelse fileExists dir then maker p
		else (pmaker dir; maker p)
	    end
	fun makedirs dir = generic (mkDir, makedirs, dir)
	fun advertisemakedirs dir =
	    (Say.vsay ["[creating directory ", dir, " ...]\n"];
	     makedirs dir)
    in
	generic (fileopener, advertisemakedirs, p)
    end

    val openTextOut = openOut TextIO.openOut
    val openBinOut = openOut BinIO.openOut
end

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
	fun mkDir d = if fileExists d then () else F.mkDir d
	fun generic (maker, pmaker, p) =
	    maker p
	    handle exn => let
		val dir = P.dir p
	    in
		if dir = "" orelse fileExists dir then raise exn
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

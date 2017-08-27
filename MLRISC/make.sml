(* make.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Regenerates all the machine description generated files.
 * This works for only 110.39+
 *)

(* force the use of the current MLRISC source tree when
 * compiling.
 *)
val () = #set(CM.symval "UNSHARED_MLRISC") (SOME 1);

fun b() = CM.make "Tools/MDL/sources.cm"; 
val _ = b();

fun c f = MDLGen.gen(f^"/"^f^".mdl");
val _ = app c [
	  "x86",
	  "amd64",
	  "sparc",
	  "alpha",
	  "hppa",
	  "ppc"
	(* , "mips" *)
	];

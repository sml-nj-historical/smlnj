(* prim-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 * This is an interface to a PrimIO structure augmented with
 * functions to create readers and writers.
 *)
signature PRIM_IO = sig

    include GENERIC_PRIM_IO

    val mkReader : {
	    fd : OS.IO.iodesc,
	    name : string,
	    chunkSize : int option  (* default not specified *) 
	  } -> reader

    val mkWriter: {
	    fd : OS.IO.iodesc,
	    name : string,
	    appendMode : bool,
	    chunkSize : int option  (* default not specified *)
	  } -> writer

end (* signature PRIM_IO *)

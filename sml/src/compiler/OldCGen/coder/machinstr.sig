(* machinstr.sig
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * This is an abstract interface to the machine instruction set.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *)

signature MACHINSTR =
sig
    datatype 'label info = INFO of {addrOf: 'label -> int, 
				    nameOf: 'label->string}

  (** Instructions **)

    type 'label instruction

    datatype ikind = IK_NOP | IK_JUMP | IK_INSTR

    val instrKind : 'label instruction -> ikind
    val nop : 'label instruction
    val latency : 'label instruction -> int


  (** Span dependent instructions **)

    type 'label sdi

    val isSdiBranch: 'a sdi -> bool
    val minSize : 'a sdi -> int
    val sizeOf : 'label info -> ('label sdi * int) -> (bool * int)
	(* sizeOf(I,loc) returns the size of I under the current address 
	 * assignment for labels plus true if the size if the maximum 
	 * possible for the sdi. 
	 *)
    val expand : 'label info -> 
		 ('label sdi * int * int) -> 'label instruction list
	(* expand (I, n, loc) returns the expansion of I into n bytes of machine
	 * instructions assuming location counter for I is loc. 
	 *)

  (** Resource usage **)

  (* 
  ** Note: needsNop should not include branch instructions.
  ** These are handled specially.
  *)
    val numResources : int
    val rUseDef : 'a instruction -> (int list * int list)
    val mayNeedNop : 'a instruction -> int
    val needsNop : ('a instruction * 'a instruction list) -> int

  (** architecture **)
    val branchDelayedArch : bool

end


(*
 * $Log: machinstr.sig,v $
 * Revision 1.1.1.1  1997/01/14 01:38:29  george
 *   Version 109.24
 *
 *)

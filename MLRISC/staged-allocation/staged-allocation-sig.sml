(* staged-allocation-sig.sml
 *
 * This code implements the Staged Allocation technique for calling conventions.
 * You can find the POPL06 paper describing this technique at
 * http://www.eecs.harvard.edu/~nr/pubs/staged-abstract.html
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 * 
 *
 * Terminology for staged allocation (see the paper for more details):
 *   counter - stores of current the number of bits allocated to the call
 *   location - a mechanism for passing a parameter, e.g., machine registers, stack locations, etc.
 *   slot - corresponds to a parameter
 *   alignment - alignment in bits for a location
 *   width - width in bits for a location
 *   stage - one rule for specifying calling conventions
 *
 *)

(* Specify the language that we wish to call. *)
signature TARGET_LANG = sig
    type location_kind     (* kind of location for passing arguments, e.g., general-purpose register, 
			    * floating-point register, memory, etc. *)
end (* TARGET_LANG *)

signature STAGED_ALLOCATION = 
  sig

    structure T : MLTREE
    structure TL : TARGET_LANG

    type width = int                                   (* bit width *)
    type counter                                       (* abstract counter for a convention *)
    type str                                           (* counter -> "bit offset" *)
    datatype block_direction = UP | DOWN               (* direction in which the overflow block grows *)
    type slot = (width * TL.location_kind * int)       (* the last field is the alignment *)
    type reg = (width * T.reg)

    (* locations consist of machine registers, offsets in to overflow blocks, combinations of
     * locations, and narrowed locations.
     *)
    datatype location =
	     REG of reg
	   | BLOCK_OFFSET of int
	   | COMBINE of (location * location)  
	   | NARROW of (location * width * TL.location_kind) 
		       
    (* metadata assocated with a location *)
    type location_info = (width * location * TL.location_kind)
			 
    (* language for specifying calling conventions *)
    datatype stage =
	     OVERFLOW of { 
	         counter : counter,
		 blockDirection : block_direction,
		 maxAlign : int 
             }
	   | WIDEN of (width -> width)
	   (* choose the first stage whose corresponding predicate is true. *)
	   | CHOICE of ( (slot -> bool) * stage) list
	   (* the first n arguments go into the first n registers *)
	   | REGS_BY_ARGS of (counter * reg list)
	   | ARGCOUNTER of counter
	   (* the first n bits arguments go into the first n bits of registers *)
	   | REGS_BY_BITS of (counter * reg list)
	   | BITCOUNTER of counter
	   (* sequence of stages *)
	   | SEQ of stage list
	   (* specifies an alignment (this rule applies even for registers) *)
	   | PAD of counter
	   (* specifies an alignment *)
	   | ALIGN_TO of (width -> width)

    (* indicates that the call-generation process has encountered an error *)
    exception StagedAlloc

    (* stepper functions take a store and an argument, and return a new store and
     * a location for passing the argument. 
     *)
    type stepper_fn = (str * slot) -> (str * location_info)

    (* Create a counter. *)
    val freshCounter : unit -> counter

    (* helper function that creates a counter c, and returns the sequence:
     * [BITCOUNTER c, REGS_BY_BITS (c, regs)] (this function is taken from 
     * the paper). 
     *)
    val useRegs : reg list -> (counter * stage)

    (* find the value stored at a counter. *)
    val find : (str * counter) -> int

    (* initialize a list of counters for a calling convention. *)
    val init : counter list -> str

    (* take a calling convention, and return a stepper function for it. *)
    val mkStep : stage list -> stepper_fn

    (* perform staged allocation over a list of slots *)
    val doStagedAllocation : (str * stepper_fn * slot list) -> (str * location_info list)

  end (* STAGED_ALLOCATION *)

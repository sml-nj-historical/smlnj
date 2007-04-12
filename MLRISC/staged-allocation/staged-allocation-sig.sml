(* staged-allocation-sig.sml
 *
 * This code implements the Staged Allocation technique for calling conventions.
 * You can find the POPL06 paper describing this technique at
 * http://www.eecs.harvard.edu/~nr/pubs/staged-abstract.html
 *)

(* the target language, e.g., C, implements this signature *)
signature TARGET_LANG = sig
    (* the type of values in the target language *)
    type location_kind
end (* TARGET_LANG *)

signature STAGED_ALLOCATION = sig

    structure T : MLTREE
    structure TL : TARGET_LANG

    type width = int     (* bit width *)
    type counter         (* abstract counter for a convention *)
    type str             (* counter -> "bit offset" *)
    datatype block_direction = UP | DOWN
    (* A slot is a target-language argument. It contains bit width, location kind, 
     * and bit alignment. *)
    type slot = (width * TL.location_kind * int)
    type reg = (width * T.reg)

    datatype location = 
	     REG of reg    (* machine register *)
	   | BLOCK_OFFSET of int     (* slot in the overflow block *)
	   (* a location that occupies two other locations*)
	   | COMBINE of (location * location)  
	   (* a location that loses bits *)  
	   | NARROW of (location * width * TL.location_kind) 
		       
    (* information about a location's width and target-language type *)
    type location_info = (width * location * TL.location_kind)
			 
    (* mini language for defining calling conventions (see the paper for 
     * a explanations of the operators, and their formal semantics). *)
    datatype stage =
	     OVERFLOW of { counter : counter,
			   blockDirection : block_direction,
			   maxAlign : int }
	   | WIDEN of (width -> width)
	   | CHOICE of ( (slot -> bool) * stage) list
	   | REGS_BY_ARGS of (counter * reg list)
	   | REGS_BY_BITS of (counter * reg list)
	   | BITCOUNTER of counter
	   | ARGCOUNTER of counter
	   | SEQ of stage list
	   | PAD of counter
	   | ALIGN_TO of (width -> width)

    exception StagedAlloc

    (* A stepper function takes a store and a target-language argument,
     * and returns a new store and a location for the argument. *)
    type stepper_fn = (str * slot) -> (str * location_info)
    (* Create a counter. *)
    val freshCounter : unit -> counter
    (* Helper function that creates a counter c, and returns the sequence:
     * [BITCOUNTER c, REGS_BY_BITS (c, regs)] (this function is taken from 
     * the paper). *)
    val useRegs : reg list -> (counter * stage)
    (* Retrieve a counter value from the store. *)
    val find : (str * counter) -> int
    (* Initialize a list of counters for a calling convention. *)
    val init : counter list -> str
    (* Take a calling convention, and return a stepper function for it. *)
    val mkStep : stage list -> stepper_fn

end (* STAGED_ALLOCATION *)

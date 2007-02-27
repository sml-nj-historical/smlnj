(* staged-allocation-sig.sml
 *
 * 
 *)

signature TARGET_LANG = sig

    type location_kind

end (* TARGET_LANG *)

signature STAGED_ALLOCATION = sig

    structure T : MLTREE
    structure TL : TARGET_LANG

    type width = int
    type counter
    type str
    datatype block_direction = UP | DOWN
                 (* bit width, location kind, bit alignment *)
    type slot = (width * TL.location_kind * int)
    type reg = (width * T.reg)

    datatype location = 
	     REG of reg         (* machine register *)
	   | BLOCK_OFFSET of int         (* slot in the overflow block *)
	   | COMBINE of (location * location)         (* a location that occupies two other locations*)
	   | NARROW of (location * width * TL.location_kind) (* a location that loses bits *)  
		       
    type location_info = (width * location * TL.location_kind)
			 
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

    type stepper_fn = (str * slot) -> (str * location_info list)

    val freshCounter : unit -> counter

    val useRegs : reg list -> (counter * stage)

    val find : (str * counter) -> int

    val init : counter list -> str

    val mkStep : stage list -> stepper_fn

    val process : {counters : counter list, stages : stage list} -> slot list
		  -> location_info list list

end (* STAGED_ALLOCATION *)

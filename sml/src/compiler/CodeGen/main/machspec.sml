(* machspec.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure DefaultMachSpec : MACH_SPEC = 
 struct

    val architecture = ""

    val numRegs = 0
    val numFloatRegs = 0
    val bigEndian = false
    val spillAreaSz = 0
    val initialSpillOffset = 0
    val startgcOffset = 0
    val pseudoRegOffset = 0
    val constBaseRegOffset = 0

    val polling = false
    val unboxedFloats = true
    val representations = true
    val newClosure = true
    val numArgRegs = 10
    val maxRepRegs = 10
    val numFloatArgRegs = 0
    val numCalleeSaves = 3
    val numFloatCalleeSaves = 0
    val untaggedInt = false

    type value_tag = {
	tagbits : int,
	tagval : int	
      }

    val intTag = {tagbits=1,tagval=1}
    val ptrTag = {tagbits=2,tagval=0}
    val descTag= {tagbits=2,tagval=2}

  (* representations of object descriptors *)
    structure ObjDesc = ObjectDesc

    val valueSize = 4
    val charSize  = 1
    val realSize  = 8
    val realAlign = true

    val quasiStack = false
    val quasiFree  = false
    val quasiFrameSz = 7

    val newListRep = false
    val listCellSz = 2

    val floatRegParams = true

    val writeAllocateHack = false

    val fixedArgPassing = false

    val spillRematerialization = false
  end (* DefaultMachSpec *)


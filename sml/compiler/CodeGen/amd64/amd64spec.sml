(* amd64spec.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AMD64Spec : MACH_SPEC = 
  struct
 
    open DefaultMachSpec

    val architecture = "amd64"
    val bigEndian = false
    val spillAreaSz = 8192
    val initialSpillOffset = 512
    val numRegs = length AMD64CpsRegs.availR (* can be |AMD64CpsRegs.availR|= + |vregs|=0 *)
    val numFloatRegs = 21   (* can be |8 upto 31| *)
    val numFloatCalleeSaves = 0
    val startgcOffset = 64
    val pseudoRegOffset = 136
    val constBaseRegOffset = 0
  (* probably should be true *)
    val fixedArgPassing = false

    val ML_STATE_OFFSET = 176
    val VProcOffMSP = 8
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

  (* number of bits and bytes per ML word (default is 32-bit architecture) *)
    val wordByteWidth = 8
    val wordBitWidth = 8*wordByteWidth

  end

structure X86Spec : MACH_SPEC = 
struct
 
  open DefaultMachSpec

  val architecture = "x86"
  val bigEndian = false
  val spillAreaSz = 8192
  val numRegs = 12 (* 18 *) (* can be |X86CpsRegs.availR| + |vregs| *)
  val numFloatRegs = 21   (* can be |8 upto 31| *)
  val numFloatCalleeSaves = 0
  val startgcOffset = 32
  val pseudoRegOffset = 136
  val constBaseRegOffset = 0
  val fixedArgPassing = false
end

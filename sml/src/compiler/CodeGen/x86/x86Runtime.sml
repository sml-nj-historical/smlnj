(* I am fed up of runtime (assembly-linkage) parameters sprinkled 
 * all over the back end.
 *)
structure X86Runtime = struct
  val numVregs = (* 24 *) 18

  (* stack offsets *)
  val vFpStart = 184			(* floating point registers  *)
  val vregStart = 72			(* virtual regs *)
  val regStart = 40			(* area for physcial registers *)
  val spillStart = 512			(* spill area *)
  val spillAreaSz = X86Spec.spillAreaSz
end

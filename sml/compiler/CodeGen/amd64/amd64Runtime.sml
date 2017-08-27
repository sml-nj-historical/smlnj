(* amd64Runtime.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AMD64Runtime =
  struct
  (* stack offsets *)
    val vFpStart = 184			(* floating point registers  *)
    val vregStart = 72			(* virtual regs *)
    val regStart = 40			(* area for physcial registers *)
    val spillStart = AMD64Spec.initialSpillOffset (* spill area *)
    val spillAreaSz = AMD64Spec.spillAreaSz
    val fpTempMemOff = 376 : Int32.int
  end

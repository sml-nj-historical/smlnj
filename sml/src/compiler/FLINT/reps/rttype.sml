(* Copyright 1998 YALE FLINT PROJECT *)
(* rttype.sml *)

signature RTTYPE = 
sig
  type tcode

  val tcode_void   : tcode
  val tcode_record : tcode
  val tcode_int32  : tcode
  val tcode_pair   : tcode
  val tcode_fpair  : tcode
  val tcode_real   : tcode
  val tcode_realN  : int -> tcode

  val tovalue      : tcode -> FLINT.value
end (* signature RTTYPE *)

structure RuntimeType :> RTTYPE = 
struct
  type tcode = int
  val tcode_void = 0
  val tcode_record = 1
  val tcode_int32 = 2
  val tcode_pair = 3
  val tcode_fpair = 4
  val tcode_real = 5
  fun tcode_realN n = n * 5
  
  fun tovalue i = FLINT.INT i
end (* structure RuntimeType *)



(*
 * $Log$
 *)

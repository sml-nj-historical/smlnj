(* alpha32Mask.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** This is temporary until we modify the runtime system with the new mask **)

structure Alpha32Mask : REGMASK = struct
  fun error msg = ErrorMsg.impossible ("Alpha32Mask." ^ msg)

  val nrRegs = 32

  val sentinel = Word.fromInt ~1
  val regMap = Array.array(32, sentinel)
  fun mapR(i,j) = Array.update(regMap,i,j)

  val _ = app mapR 
	       [( 3, 0w0),	(* stdlink *)
		( 2, 0w1),	(* stdclos *)
		( 0, 0w2),	(* stdarg *)
		( 1, 0w3),	(* stdcont *)
		( 6, 0w4),	(* misc0 *)
		( 7, 0w5),	(* misc1 *)
		( 8, 0w6),	(* ...   *)
		(16, 0w7),
		(17, 0w8),
		(18, 0w9),
		(19, 0w10),
		(20, 0w11),
		(21, 0w12),
		(22, 0w13),
		(23, 0w14),
		(24, 0w15),
		(25, 0w16),
		(27, 0w17)]

  fun regMask(reg, mask) = let
    val bit = Array.sub(regMap, reg)
  in
    if bit=sentinel then error ("regMask - " ^ Int.toString reg)
    else Word.orb(mask,Word.<<(0w1,bit))
  end

  fun memMask _ = error "memMask"
end


(*
 * $Log: alpha32Mask.sml,v $
 * Revision 1.2  1997/07/17 12:35:23  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
# Revision 1.1  1997/04/19  18:17:44  george
#   Version 109.27
#
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)

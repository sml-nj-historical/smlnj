(* hppaMask.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure HppaMask : REGMASK = struct

  fun error msg = ErrorMsg.impossible ("HppaMask." ^ msg)

  val sentinel = Word.fromInt ~1
  val regMap = Array.array(32, sentinel)
  val _ = app (fn (i,j) => Array.update(regMap,i,j))
	       [( 9, 0w0),	(* stdlink *)
		(10, 0w1),	(* stdclos *)
		(11, 0w2),	(* stdarg *)
		(12, 0w3),	(* stdcont *)
		( 1, 0w4),	(* misc0 *)
		(13, 0w5),	(* misc1 *)
		(14, 0w6),	(* ...   *)
		(15, 0w7),
		(16, 0w8),
		(17, 0w9),
		(18, 0w10),
		(19, 0w11),
		(22, 0w12),
		(23, 0w13),
		(24, 0w14),
		(25, 0w15),
		(26, 0w16),
		(28, 0w17),
		(2, 0w18)]
	handle _ => error "setting regMap"


  fun regMask(reg, mask) = let
    val bit = Array.sub(regMap, reg)
  in
    if bit= sentinel then error ("regMask - " ^ Int.toString reg)
    else Word.orb(mask,Word.<<(0w1,bit))
  end

  fun memMask _ = error "memMask"
end


(*
 * $Log: hppaMask.sml,v $
 * Revision 1.2  1997/07/17 12:37:46  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
# Revision 1.1  1997/04/19  18:17:49  george
#   Version 109.27
#
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)

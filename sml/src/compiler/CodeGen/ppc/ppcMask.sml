(* ppcMask.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)
structure PPCMask : REGMASK = struct
  fun error msg = ErrorMsg.impossible ("PPCMask." ^ msg)

  val nrRegs = 32

  val sentinel = Word.fromInt ~1
  val regMap = Array.array(32, sentinel)
  fun mapR(i,j) = Array.update(regMap,i,j)

  val _ = app mapR 
	       [(17, 0w0),	(* stdlink *)
		(18, 0w1),	(* stdclos *)
		(19, 0w2),	(* stdarg *)
		(20, 0w3),	(* stdcont *)
		(24, 0w4),	(* misc0 *)
		(25, 0w5),	(* misc1 *)
		(26, 0w6),
		(27, 0w7),
		( 3, 0w8),
		( 4, 0w9),
		( 5, 0w10),
		( 6, 0w11),
		( 7, 0w12),
		( 8, 0w13),
		( 9, 0w14),
		(10, 0w15),
		(11, 0w16),
		(12, 0w17),
		(13, 0w18)]


  fun regMask(reg, mask) = let
    val bit = Array.sub(regMap, reg)
  in
    if bit=sentinel then error ("regMask - " ^ Int.toString reg)
    else Word.orb(mask,Word.<<(0w1,bit))
  end

  fun memMask _ = error "memMask"
end


(* sparcMask.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

structure SparcMask : REGMASK = struct

  fun error msg = ErrorMsg.impossible ("SparcMask." ^ msg)

  val sentinel = Word.fromInt ~1
  val regMap = Array.array(32, sentinel)
  val _ = app (fn (i,j) => Array.update(regMap,i,j))
	       [( 1, 0w0),	(* stdlink *)
		(26, 0w1),	(* stdclos *)
		(24, 0w2),	(* stdarg *)
		(25, 0w3),	(* stdcont *)
		( 2, 0w4),	(* misc0 *)
		( 3, 0w5),	(* misc1 *)
		( 8, 0w6),	(* ...   *)
		( 9, 0w7),
		(16, 0w8),
		(17, 0w9),
		(18, 0w10),
		(19, 0w11),
		(20, 0w12),
		(21, 0w13),
		(22, 0w14),
		(23, 0w15),
		(28, 0w16),
		(11, 0w17),
		(12, 0w18)
		]

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
 * $Log$
 *)

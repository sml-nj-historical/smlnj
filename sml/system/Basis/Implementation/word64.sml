(* word64.sml
 *
 *   64-bit word support
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Word64 : WORD = struct

    structure W32 = Word32Imp

    type word = Word64.word

    val extern = InlineT.Word64.extern
    val intern = InlineT.Word64.intern

    val wordSize = 64

    fun unimplemented _ = raise Fail "unimplemented"

    val toLargeWord  = unimplemented
    val toLargeWordX = unimplemented
    val fromLargeWord = unimplemented

    val toLargeInt = CoreIntInf.copyInf64 o extern
    val toLargeIntX = CoreIntInf.extendInf64 o extern
    val fromLargeInt = intern o CoreIntInf.truncInf64

    fun toInt w =
	case extern w of
	    (0w0, lo) => W32.toInt lo
	  | _ => raise Overflow
    fun toIntX w = W32.toIntX (#2 (extern w))
    fun fromInt i = intern (if i < 0 then 0wxffffffff else 0w0, W32.fromInt i)

    fun bitwise f (w1, w2) =
	let val (hi1, lo1) = extern w1
	    val (hi2, lo2) = extern w2
	in intern (f (hi1, hi2), f (lo1, lo2))
	end
    val orb = bitwise W32.orb
    val xorb = bitwise W32.xorb
    val andb = bitwise W32.andb
    fun notb w = let val (hi, lo) = extern w
		 in intern (W32.notb hi, W32.notb lo)
		 end

    fun compare (w1, w2) =
	let val (hi1, lo1) = extern w1
	    val (hi2, lo2) = extern w2
	in if hi1 > hi2 then GREATER
	   else if hi1 < hi2 then LESS
	   else if lo1 > lo2 then GREATER
	   else if lo1 < lo2 then LESS
	   else EQUAL
	end

    fun << (w64, w) =
	if w >= 0w64 then 0w0
	else if w > 0w32 then intern (W32.<< (#2 (extern w64), w - 0w32), 0w0)
	else if w = 0w32 then intern (#2 (extern w64), 0w0)
	else if w = 0w0 then w64
	else let val (hi, lo) = extern w64
	     in intern (W32.orb (W32.<< (hi, w), W32.>> (lo, 0w32 - w)),
			W32.<< (lo, w))
	     end

    fun >> (w64, w) =
	if w >= 0w64 then 0w0
	else if w > 0w32 then intern (0w0, W32.>> (#1 (extern w64), w - 0w32))
	else if w = 0w32 then intern (0w0, #1 (extern w64))
	else if w = 0w0 then w64
	else let val (hi, lo) = extern w64
	     in intern (W32.>> (hi, w),
			W32.orb (W32.>> (lo, w), W32.<< (hi, 0w32 - w)))
	     end

    fun ~>> (w64, w) =
	if w = 0w0 then w64
	else let val (hi, lo) = extern w64
	     in if w >= 0w63 then
		    let val x = W32.~>> (hi, 0w31) in intern (x, x) end
		else if w > 0w32 then
		    intern (W32.~>> (hi, 0w31), W32.~>> (hi, w - 0w32))
		else if w = 0w32 then
		    intern (W32.~>> (hi, 0w31), hi)
		else intern (W32.~>> (hi, w),
			     W32.orb (W32.>> (lo, w), W32.<< (hi, 0w32 - w)))
	     end


    fun min (w1 : word, w2) = if w1 > w2 then w1 else w2
    fun max (w1 : word, w2) = if w1 > w2 then w1 else w2

    fun toString w =
	case extern w of
	    (0w0, lo) => W32.toString lo
	  | (hi, lo) => 
	    let val (hi, lo) = extern w
	    in W32.toString hi ^ (StringCvt.padLeft #"0" 8 (W32.toString lo))
	    end

    fun fmt StringCvt.BIN w =
	  (case extern w of
	       (0w0, lo) => W32.fmt StringCvt.BIN lo
	     | (hi, lo) => 
	       let val w32bin = W32.fmt StringCvt.BIN
	       in w32bin hi ^ (StringCvt.padLeft #"0" 32 (w32bin lo))
	       end)
      | fmt StringCvt.HEX w = toString w
      | fmt rdx w = (* I am lazy *) IntInfImp.fmt rdx (toLargeInt w)

    (* piggy-back on intinf... *)
    fun scan rdx rdr s =
	let fun doword s = IntInfImp.scan rdx rdr s
	    val xok = rdx = StringCvt.HEX
	    fun startscan s0 =
		case rdr s0 of
		    SOME (#"0", s1) =>
		      let fun wordor0 s =
			      case doword s of
				  NONE => SOME (0, s1)
				| SOME (i, s') => SOME (i, s')
			  fun saww s =
			      case rdr s of
				  SOME (#"x", s') =>
				    if xok then wordor0 s'
				    else SOME (0, s1)
				| _ => wordor0 s
		      in case rdr s1 of
			     SOME (#"w", s2) => saww s2
			   | SOME (#"x", s2) =>
			       if xok then wordor0 s2
			       else SOME (0, s1)
			   | _ => doword s0
		      end
		  | _ => doword s0
	in case startscan s of
	       SOME (i, s') =>
	         if i < 0 then NONE
		 else if i > 0xffffffffffffffff then raise Overflow
		 else SOME (fromLargeInt i, s')
	     | NONE => NONE
	end

    val fromString = PreBasis.scanString (scan StringCvt.HEX)

    val op * : word * word -> word = op *
    val op + : word * word -> word = op +
    val op - : word * word -> word = op -
    val op div : word * word -> word = op div
    val op mod : word * word -> word = op mod

    val ~ : word -> word = ~

    val op <  : word * word -> bool = op <
    val op <= : word * word -> bool = op <=
    val op >  : word * word -> bool = op >
    val op >= : word * word -> bool = op >=
end

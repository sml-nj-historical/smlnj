(* base64.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for Base64 encoding/decoding as specified by RFC 4648.
 *
 *	http://www.ietf.org/rfc/rfc4648.txt
 *)

structure Base64 : BASE64 =
  struct

    structure W8 = Word8
    structure W8V = Word8Vector
    structure W8A = Word8Array
    structure UCV = Unsafe.CharVector
    structure UW8V = Unsafe.Word8Vector

  (* encoding table *)
    val encTbl = "\
	    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
	    \abcdefghijklmnopqrstuvwxyz\
	    \0123456789+/\
	  \"
    val padChar = #"="
    fun incByte b = UCV.sub(encTbl, Word8.toIntX b) 

  (* encode a triple of bytes into four base-64 characters *)
    fun encode3 (b1, b2, b3) = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.orb(W8.<<(W8.andb(b1, 0wx3), 0w4), W8.>>(b2, 0w4))
	  val c3 = W8.orb(W8.<<(W8.andb(0wxF, b2), 0w2), W8.>>(b3, 0w6))
	  val c4 = W8.andb(0wx3f, b3)
	  in
	    (incByte c1, incByte c2, incByte c3, incByte c4)
	  end

  (* encode a pair of bytes into three base-64 characters plus a padding character *)
    fun encode2 (b1, b2) = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.orb(W8.<<(W8.andb(b1, 0wx3), 0w4), W8.>>(b2, 0w4))
	  val c3 = W8.<<(W8.andb(0wxF, b2), 0w2)
	  in
	    (incByte c1, incByte c2, incByte c3, padChar)
	  end

  (* encode a byte into two base-64 characters plus two padding characters *)
    fun encode1 b1 = let
	  val c1 = W8.>>(b1, 0w2)
	  val c2 = W8.<<(W8.andb(b1, 0wx3), 0w4)
	  in
	    (incByte c1, incByte c2, padChar, padChar)
	  end

    local
      fun encode (vec, start, len) = let
	    val outLen = 4 * Int.quot(len + 2, 3)
	    val outBuf = Unsafe.CharVector.create outLen
	    val nTriples = Int.quot(len, 3)
	    val extra = Int.rem(len, 3)
	    fun insBuf (i, (c1, c2, c3, c4)) = let
		  val idx = 4*i
		  in
		    UCV.update(outBuf, idx,   c1);
		    UCV.update(outBuf, idx+1, c2);
		    UCV.update(outBuf, idx+2, c3);
		    UCV.update(outBuf, idx+3, c4)
		  end
	    fun loop (i, idx) = if (i < nTriples)
		  then (
		    insBuf(i, encode3(UW8V.sub(vec, idx), UW8V.sub(vec, idx+1), UW8V.sub(vec, idx+2)));
		    loop (i+1, idx+3))
		  else (case extra
		     of 1 => insBuf(i, encode1(UW8V.sub(vec, idx)))
		      | 2 => insBuf(i, encode2(UW8V.sub(vec, idx), UW8V.sub(vec, idx+1)))
		      | _ => ()
		    (* end case *))
	    in
	      loop (0, start);
	      outBuf
	    end
    in

    fun encodeVec vec = encode (vec, 0, W8V.length vec)

    fun encodeVecSlice slice = encode (Word8VectorSlice.base slice)

    end (* local *)

  (* decoding tags *)
    val errCode : W8.word = 0w255
    val padCode : W8.word = 0w65
    val spCode : W8.word = 0w66
    val decTbl = let
	  val tbl = W8A.array(256, errCode)
	  fun ins (w, c) = W8A.update(tbl, Char.ord c, w)
	  in
	  (* add space codes *)
	    ins(spCode, #"\t");
	    ins(spCode, #"\n");
	    ins(spCode, #"\r");
	    ins(spCode, #" ");
	  (* add decoding codes *)
	    CharVector.appi (fn (i, c) => ins(Word8.fromInt i, c)) encTbl;
	  (* convert to vector *)
	    W8V.tabulate (256, fn i => W8A.sub(tbl, i))
	  end

  end

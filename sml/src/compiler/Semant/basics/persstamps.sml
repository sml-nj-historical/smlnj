(* Copyright 1992 by AT&T Bell Laboratories *)
(* persstamps.sml *)

(* 
 * DBM: was an abstraction, but persstamp needs to be an equality type 
 * so that conrep is. 
 *)

structure PersStamps : PERSSTAMPS =
struct
  datatype persstamp = PS of Word8Vector.vector

  fun compare (PS v1, PS v2) =
        String.compare(Byte.bytesToString v1, Byte.bytesToString v2)

  fun toBytes (PS x) = x

  fun fromBytes v = 
    if Word8Vector.length v = 16 then PS v
    else ErrorMsg.impossible "PersStamps.stringToStamp"

  (* convert the persstamp to a printable representation (hex digits) *)
  fun toHex (PS pid) = 
    let fun cvtByte b = StringCvt.padLeft #"0" 2 (Word8.toString b)
	fun f (b, l) = cvtByte b :: l
     in String.concat (Word8Vector.foldr f [] pid)
    end

end (* structure PersStamps *)




(* bug1497.1.sml *)

let val str = "0x1"
    fun reader offset =
	if offset = size str
	then NONE
	else SOME (String.sub(str, offset), offset + 1)
 in IntInf.toString(#1(valOf(IntInf.scan StringCvt.HEX reader 0)))
end

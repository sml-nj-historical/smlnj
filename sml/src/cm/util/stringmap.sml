(*
 * String dictionaries.
 *   Hooks into compiler and uses SML/NJ library implementation of binary maps.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure StringMap = BinaryMapFn
    (struct
	type ord_key = string
	val compare = String.compare
    end)

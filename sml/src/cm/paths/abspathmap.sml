(*
 * AbsPath dictionaries.
 *   Uses SML/NJ library implementation of binary maps.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure AbsPathMap = BinaryMapFn
    (struct
	type ord_key = AbsPath.t
	val compare = AbsPath.compare
    end)

(*
 * AbsPath sets.
 *   Uses SML/NJ library implementation of sets.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure AbsPathSet = BinarySetFn
    (struct
	type ord_key = AbsPath.t
	val compare = AbsPath.compare
    end)

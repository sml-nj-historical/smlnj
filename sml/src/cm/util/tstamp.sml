(*
 * CM timestamp semantics.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure TStamp = struct

    datatype t =
	NOTSTAMP
      | TSTAMP of Time.time

    val ancient = TSTAMP (Time.zeroTime)

    (*
     * If f1 depends on f2, then earlier (modtime f1, modtime f2) implies
     * that f1 needs to be recompiled...     *
     *)
    fun earlier (_, NOTSTAMP) = false	(* prerequisite missing *)
      | earlier (NOTSTAMP, _) = true	(* object missing *)
      | earlier (TSTAMP t1, TSTAMP t2) = Time.< (t1, t2)
end

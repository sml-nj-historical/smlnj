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

    (* We consider a target good if it has the same time stamp
     * as the source.  A target that isn't there is never good,
     * and if there is a target but no source, then we assume the
     * target to be ok. *)
    fun needsUpdate { target = NOTSTAMP, ... } = true
      | needsUpdate { source = NOTSTAMP, ... } = false
      | needsUpdate { source = TSTAMP st, target = TSTAMP tt } =
	Time.compare (st, tt) <> EQUAL
end

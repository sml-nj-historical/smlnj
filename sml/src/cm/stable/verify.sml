(*
 * Verifying the validity of an existing stable file for a (non-stable)
 * library.
 *   - This is used for "paranoia" mode during bootstrap compilation.
 *     Normally, CM takes stable files and doesn't ask questions, but
 *     during bootstrap compilation it takes the stable file only if
 *     it is verified to be valid.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GG = GroupGraph
    structure GP = GeneralParams
    structure TS = TStamp
in
signature VERIFY_STABLE = sig
    val verify : GP.info -> GG.group -> bool
end

structure VerifyStable :> VERIFY_STABLE = struct
    fun verify (gp: GP.info) g = let
	val policy = #fnpolicy (#param gp)
	fun sname p = FilenamePolicy.mkStableName policy p
	val GG.GROUP { grouppath, exports, sublibs, ... } = g
	val stablename = sname grouppath
	fun invalidSublib st (p, GG.GROUP { kind = GG.STABLELIB _, ... }) =
	    let val sn = sname p
	    in case TS.fmodTime sn of
		TS.TSTAMP t => Time.compare (t, st) = GREATER
	      | _ => true
	    end
	  | invalidSublib _ _ = true
	fun invalidMember stab_t i = let
	    val p = SmlInfo.sourcepath i
	    val bn = SmlInfo.binname i
	in
	    case (SrcPath.tstamp p, TS.fmodTime bn) of
		(TS.TSTAMP src_t, TS.TSTAMP bin_t) =>
		    Time.compare (src_t, bin_t) <> EQUAL orelse
		    Time.compare (src_t, stab_t) = GREATER
	      | _ => true
	end
    in
	case (TS.fmodTime stablename, SrcPath.tstamp grouppath) of
	    (TS.TSTAMP st, TS.TSTAMP gt) =>
		if Time.compare (st, gt) = LESS then false
		else not (SmlInfoSet.exists (invalidMember st)
			                    (Reachable.reachable g) orelse
			  List.exists (invalidSublib st) sublibs)
	  | _ => false
    end
end
end

(*
 * The "generic" compilation traversal functor.
 *  (In fact, it is probably possible to use this for things other
 *   than compilation as well.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
    structure GG = GroupGraph
in
    functor CompileGenericFn (structure CT: COMPILATION_TYPE) :> sig

	type envdelta = CT.envdelta
	type result = CT.result

	val bnode : GP.info -> DG.bnode -> envdelta option
	val snode : GP.info -> DG.snode -> envdelta option
	val group : GP.info -> GG.group -> result option

    end = struct

	type envdelta = CT.envdelta
	type env = CT.env
	type benv = CT.benv
	type result = CT.result

	(* This is to prevent re-execution of dosml if the first one failed *)
	local
	    val failures = ref SmlInfoSet.empty
	in
	    fun dosml (i, e, gp) =
		if SmlInfoSet.member (!failures, i) then NONE
		else case CT.dosml (i, e, gp) of
		    SOME r => SOME r
		  | NONE => (failures := SmlInfoSet.add (!failures, i); NONE)
	    fun clearFailures () = failures := SmlInfoSet.empty
	end

	(* To implement "keep_going" we have two different ways of
	 * combining a "work" function with a "layer" function.
	 * One way is to give up and do no further work once there
	 * is a result of NONE, the other one is to continue
	 * working (but to ignore the results of such work). *)
	fun layerwork (k, layer, work) (x, NONE) =
	    (if k then ignore (work x) else (); NONE)
	  | layerwork (k, layer, work) (x, SOME e) =
	    case work x of
		NONE => NONE
	      | SOME e' => SOME (layer (e', e))

	fun bnode (gp: GP.info) n = let

	    val k = #keep_going (#param gp)
	    val glob = foldl (layerwork (k, CT.blayer, farbnode gp))
	    val loc =
		foldl (layerwork (k, CT.blayer,
				  Option.map CT.bnofilter o bnode gp))

	    fun bn (DG.PNODE p) = SOME (CT.primitive gp p)
	      | bn (DG.BNODE n) = let
		    val { bininfo, localimports = li, globalimports = gi } = n
		    fun mkenv () = loc (glob (SOME (CT.bpervasive gp)) gi) li
		in
		    CT.dostable (bininfo, mkenv, gp)
		end
	in
	    (* don't eta-reduce this -- it'll lead to an infinite loop! *)
	    bn n
	end

	and farbnode gp (f, n) =
	    case (bnode gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.bnofilter d)
	      | (SOME d, SOME s) => SOME (CT.bfilter (d, s))

	fun snode gp (DG.SNODE n) = let

	    val k = #keep_going (#param gp)
	    val glob =
		foldl (layerwork (k, CT.layer, farsbnode gp))
	    val loc =
		foldl (layerwork (k, CT.layer,
				  Option.map CT.nofilter o snode gp))

	    val { smlinfo, localimports = li, globalimports = gi } = n
	    val desc = SmlInfo.fullSpec smlinfo
	    val pe = SOME (CT.pervasive gp)
	    val ge = glob pe gi
	    val e = loc ge li
	in
	    case e of
		NONE => NONE
	      | SOME e => dosml (smlinfo, e, gp)
	end

	and sbnode gp (DG.SB_BNODE b) = bnode gp b
	  | sbnode gp (DG.SB_SNODE s) = snode gp s

	and farsbnode gp (f, n) =
	    case (sbnode gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.nofilter d)
	      | (SOME d, SOME s) => SOME (CT.filter (d, s))

	fun impexp gp (n, _) = Option.map CT.env2result (farsbnode gp n)

	fun group gp (GG.GROUP { exports, ... }) =
	    (foldl (layerwork (#keep_going (#param gp),
		               CT.rlayer,
			       impexp gp))
	           (SOME CT.empty)
		   (SymbolMap.listItems exports))
	    before clearFailures ()
    end
end

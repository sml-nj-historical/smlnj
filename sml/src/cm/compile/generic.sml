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
    functor CompileGenericFn (structure CT: COMPILATION_TYPE
			      val thinTraversal: bool) :> TRAVERSAL
	where type envdelta = CT.envdelta
	  and type result = CT.result =
    struct

	type envdelta = CT.envdelta
	type env = CT.env
	type benv = CT.benv
	type result = CT.result

	val smlcache = ref (SmlInfoMap.empty: envdelta option SmlInfoMap.map)
	val stablecache = ref (StableMap.empty: envdelta option StableMap.map)
	fun reset () = (CT.nestedTraversalReset ();
			smlcache := SmlInfoMap.empty)
	fun resetAll () = (reset (); stablecache := StableMap.empty)

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

	fun thinOut l = let
	    fun one ((n, false), l) = l
	      | one ((n, true), l) = n :: l
	in
	    foldr one [] l
	end

	fun dontThinOut l = map (fn (n, _) => n) l

	fun bnode (gp: GP.info) n = let

	    val k = #keep_going (#param gp)
	    val glob = foldl (layerwork (k, CT.blayer, farbnode gp))
	    val loc =
		foldl (layerwork (k, CT.blayer,
				  Option.map CT.bnofilter o bnode gp))

	    fun bn (DG.PNODE p) = SOME (CT.primitive gp p)
	      | bn (node as DG.BNODE n) = let
		    val { bininfo, localimports = li, globalimports = gi } = n
		    val (li, gi) =
			if thinTraversal then (thinOut li, thinOut gi)
			else (dontThinOut li, dontThinOut gi)
		in
		    case StableMap.find (!stablecache, bininfo) of
			SOME r => r
		      | NONE => let
			    fun mkenv () =
				loc (glob (SOME (CT.bpervasive gp)) gi) li
			    val r = CT.dostable (bininfo, mkenv, gp, node)
			in
			    stablecache :=
			       StableMap.insert (!stablecache, bininfo, r);
			    r
			end
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

	fun snode gp (node as DG.SNODE n) = let

	    val k = #keep_going (#param gp)
	    val glob = foldl (layerwork (k, CT.layer, sglobi gp))
	    val loc = foldl (layerwork (k, CT.layer, sloci gp))

	    val i = #smlinfo n
	in
	    case SmlInfoMap.find (!smlcache, i) of
		SOME r => r
	      | NONE => let
		    val pe = SOME (CT.pervasive gp)
		    val ge = glob pe (#globalimports n)
		    val e = loc ge (#localimports n)
		    val r = case e of
			NONE => NONE
		      | SOME e => CT.dosml (i, e, gp, node)
		in
		    smlcache := SmlInfoMap.insert (!smlcache, i, r);
		    r
		end
	end

	and sglobi gp (n, r) =
	    Option.map (CT.withAccessTrap r) (farsbnode gp n)
	and sloci gp (n, r) =
	    Option.map (CT.withAccessTrap r o CT.nofilter) (snode gp n)

	and sbnode gp (DG.SB_BNODE b) = bnode gp b
	  | sbnode gp (DG.SB_SNODE s) = snode gp s

	and farsbnode gp (f, n) =
	    case (sbnode gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.nofilter d)
	      | (SOME d, SOME s) => SOME (CT.filter (d, s))

	fun impexp gp (n, _) = Option.map CT.env2result (farsbnode gp n)

	fun impexpmap gp m =
	    (foldl (layerwork (#keep_going (#param gp),
		               CT.rlayer,
			       impexp gp))
	           (SOME CT.empty)
		   (SymbolMap.listItems m))
	    before reset ()

	fun group gp (GG.GROUP { exports, ... }) = impexpmap gp exports
    end
end

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
    functor CompileGenericFn (structure CT: COMPILATION_TYPE) :> TRAVERSAL
	where type envdelta = CT.envdelta
	  and type result = CT.result =
    struct

	type envdelta = CT.envdelta
	type env = CT.env
	type benv = CT.benv
	type result = CT.result

	type ts = CT.ts * envdelta option SmlInfoMap.map ref
	type tsnode = DependencyGraph.farsbnode * ts

	val stablecache = ref (StableMap.empty: envdelta option StableMap.map)

	fun reset () = stablecache := StableMap.empty

	fun start () = (CT.start (), ref SmlInfoMap.empty)
	fun finish (ctts, _) = CT.finish ctts

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

	fun bnode ts (gp: GP.info) n = let

	    val (ctts, _) = ts
	    val k = #keep_going (#param gp)
	    val glob = foldl (layerwork (k, CT.blayer, farbnode ts gp))
	    val loc =
		foldl (layerwork (k, CT.blayer,
				  Option.map CT.bnofilter o bnode ts gp))

	    fun bn (DG.PNODE p) = SOME (CT.primitive gp p)
	      | bn (node as DG.BNODE n) = let
		    val { bininfo, localimports = li, globalimports = gi } = n
		in
		    case StableMap.find (!stablecache, bininfo) of
			SOME r => r
		      | NONE => let
			    fun mkenv () =
				loc (glob (SOME (CT.bpervasive gp)) gi) li
			    val r =
				CT.dostable (bininfo, mkenv, gp, node, ctts)
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

	and farbnode ts gp (f, n) =
	    case (bnode ts gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.bnofilter d)
	      | (SOME d, SOME s) => SOME (CT.bfilter (d, s))

	fun snode ts gp (node as DG.SNODE n) = let

	    val (ctts, smlcache) = ts
	    val k = #keep_going (#param gp)
	    val glob = foldl (layerwork (k, CT.layer, farsbnode ts gp))
	    val loc =
		foldl (layerwork (k, CT.layer,
				  Option.map CT.nofilter o snode ts gp))
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
		      | SOME e => CT.dosml (i, e, gp, node, ctts)
		in
		    smlcache := SmlInfoMap.insert (!smlcache, i, r);
		    r
		end
	end

	and sbnode ts gp (DG.SB_BNODE b) = bnode ts gp b
	  | sbnode ts gp (DG.SB_SNODE s) = snode ts gp s

	and farsbnode ts gp (f, n) =
	    case (sbnode ts gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.nofilter d)
	      | (SOME d, SOME s) => SOME (CT.filter (d, s))

	fun resume1 gp (n, ts) =
	    Option.map CT.env2result (farsbnode ts gp n)
	    before finish ts

	fun resume getter gp m =
	    foldl (layerwork (#keep_going (#param gp),
			      CT.rlayer,
			      resume1 gp o getter))
	          (SOME CT.empty)
		  (SymbolMap.listItems m)

	fun group gp (GG.GROUP { exports, ... }) = let
	    val ts = start ()
	    fun getter (n, _) = (n, ts)
	in
	    resume getter gp exports
	end

	fun withNewTs f gp n = let
	    val ts = start ()
	in
	    f ts gp n before finish ts
	end

	val bnode' = withNewTs bnode
	val snode' = withNewTs snode
    end
end

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

	(* To implement "keep_going" we have two different ways to "fold"
	 * a "layer" function over a list.  The _k version is to be used
	 * if keep_going is true, otherwise the _s version applies.
	 * Note that there is a bit of typing mystery in the way I use
	 * these functions later: I had to be more verbose than I wanted
	 * to because of the "value restriction rule" in SML'97. *)
	fun foldlayer_k layer f = let
	    fun loop r [] = r
	      | loop NONE (h :: t) = (ignore (f h); loop NONE t)
	      | loop (SOME e) (h :: t) =
		case f h of
		    NONE => loop NONE t
		  | SOME e' => loop (SOME (layer (e', e))) t
	in
	    loop
	end

	fun foldlayer_s layer f NONE l = NONE
	  | foldlayer_s layer f (SOME i) l = let
		fun loop e [] = SOME e
		  | loop e (h :: t) =
		    case f h of
			NONE => NONE
		      | SOME e' => loop (layer (e', e)) t
	    in
		loop i l
	    end

	fun bnode (gp: GP.info) n = let

	    val (glob, loc) = let
		val globf = farbnode gp
		val locf = Option.map CT.bnofilter o bnode gp
		fun k f = foldlayer_k CT.blayer f
		fun s f = foldlayer_s CT.blayer f
	    in
		if #keep_going (#param gp) then (k globf, k locf)
		else (s globf, s locf)
	    end

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

	    val (glob, loc) = let
		val globf = farsbnode gp
		val locf = Option.map CT.nofilter o snode gp
		fun k f = foldlayer_k CT.layer f
		fun s f = foldlayer_s CT.layer f
	    in
		if #keep_going (#param gp) then (k globf, k locf)
		else (s globf, s locf)
	    end

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

	fun group gp (GG.GROUP { exports, ... }) = let
	    val fl =
		if #keep_going (#param gp) then foldlayer_k else foldlayer_s
	in
	    (fl CT.rlayer (impexp gp)
	                  (SOME CT.empty)
			  (SymbolMap.listItems exports))
	    before clearFailures ()
	end
    end
end

(*
 * The "generic" compilation traversal functor.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure GP = GeneralParams
    structure DG = DependencyGraph
in
    functor CompileGenericFn (structure CT: COMPILATION_TYPE) :> sig

	type envdelta = CT.envdelta
	type benv = CT.benv
	type env = CT.env

	val bnode : GP.params -> DG.bnode -> envdelta option
	val farbnode : GP.params -> DG.farbnode -> benv option
	val snode : GP.params -> DG.snode -> envdelta option
	val sbnode : GP.params -> DG.sbnode -> envdelta option
	val farsbnode : GP.params -> DG.farsbnode -> env option

    end = struct

	type envdelta = CT.envdelta
	type env = CT.env
	type benv = CT.benv

	fun prim (gp: GP.params) = CT.primitive (#primconf gp)

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

	fun bnode (gp: GP.params) = let

	    val (glob, loc) = let
		val globf = farbnode gp
		val locf = Option.map CT.bnofilter o bnode gp
		fun k f = foldlayer_k CT.blayer f
		fun s f = foldlayer_s CT.blayer f
	    in
		if #keep_going gp then (k globf, k locf)
		else (s globf, s locf)
	    end

	    fun bn (DG.PNODE p) = SOME (prim gp p)
	      | bn (DG.BNODE n) = let
		    val { bininfo, localimports = li, globalimports = gi } = n
		    fun mkenv () = let
			val pe = CT.bnofilter (prim gp Primitive.pervasive)
			val ge = glob (SOME pe) gi
		    in
			loc ge li
		    end
		in
		    CT.dostable (bininfo, mkenv, gp)
		end
	in
	    bn
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
		if #keep_going gp then (k globf, k locf)
		else (s globf, s locf)
	    end

	    val { smlinfo, localimports = li, globalimports = gi } = n
	    val pe = CT.nofilter (prim gp Primitive.pervasive)
	    val ge = glob (SOME pe) gi
	    val le = loc ge li
	in
	    case le of
		NONE => NONE
	      | SOME le => CT.dosml (smlinfo, le, gp)
	end

	and sbnode gp (DG.SB_BNODE b) = bnode gp b
	  | sbnode gp (DG.SB_SNODE s) = snode gp s

	and farsbnode gp (f, n) =
	    case (sbnode gp n, f) of
		(NONE, _) => NONE
	      | (SOME d, NONE) => SOME (CT.nofilter d)
	      | (SOME d, SOME s) => SOME (CT.filter (d, s))
    end
end

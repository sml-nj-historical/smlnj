(*
 * The result signature of the "generic" compilation traversal functor.
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
    signature TRAVERSAL = sig
	type envdelta
	type result

	val bnode : GP.info -> DG.bnode -> envdelta option
	val group : GP.info -> GG.group -> result option
	val impexpmap :
	    GP.info -> DependencyGraph.impexp SymbolMap.map -> result option

	(* If you go through the "sbnode" or "snode" interface, then
	 * you must reset explicitly when you are done. *)
	val sbnode : GP.info -> DG.sbnode -> envdelta option
	val snode : GP.info -> DG.snode -> envdelta option
	val reset : unit -> unit

	val resetAll : unit -> unit
    end
end

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

	type ts				(* traversal state *)

	val start : unit -> ts
	val finish : ts -> unit

	val group : GP.info -> GG.group -> result option
	val bnode' : GP.info -> DG.bnode -> envdelta option
	val snode' : GP.info -> DG.snode -> envdelta option

	val sbnode : ts -> GP.info -> DG.sbnode -> envdelta option

	val resume : ('a -> DependencyGraph.farsbnode * ts) ->
	    GP.info -> 'a SymbolMap.map -> result option

	val reset : unit -> unit
    end
end

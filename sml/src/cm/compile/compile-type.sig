(*
 * Argument signature for the "generic" compilation traversal functor.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature COMPILATION_TYPE = sig

    type env
    type benv
    type envdelta

    val layer : env * env -> env
    val blayer : benv * benv -> benv

    val filter : envdelta * SymbolSet.set -> env
    val bfilter : envdelta * SymbolSet.set -> benv

    val nofilter : envdelta -> env
    val bnofilter : envdelta -> benv

    val primitive : Primitive.configuration -> Primitive.primitive -> envdelta

    val dostable:
	BinInfo.info * (unit -> benv option) * GeneralParams.params
	-> envdelta option

    val dosml :
	SmlInfo.info * env * GeneralParams.params
	-> envdelta option
end

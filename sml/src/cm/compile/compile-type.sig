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
    type result

    val empty : result
    val env2result : env -> result

    val layer : env * env -> env
    val blayer : benv * benv -> benv
    val rlayer : result * result -> result

    val filter : envdelta * SymbolSet.set -> env
    val bfilter : envdelta * SymbolSet.set -> benv

    val nofilter : envdelta -> env
    val bnofilter : envdelta -> benv

    val primitive : GeneralParams.info -> Primitive.primitive -> envdelta
    val bpervasive : GeneralParams.info -> benv
    val pervasive : GeneralParams.info -> env

    val dostable:
	BinInfo.info * (unit -> benv option) * GeneralParams.info
	-> envdelta option

    val dosml :	SmlInfo.info * env * GeneralParams.info -> envdelta option
end

(*
 * Bundling information pertaining to the member of a stable group.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BININFO = sig

    type info
    type complainer = GenericVC.ErrorMsg.complainer

    val compare : info * info -> order
    val describe : info -> string
    val offset : info -> int
    val group : info -> AbsPath.t
    val share : info -> bool option
    val error : GeneralParams.params -> info -> complainer
end

structure BinInfo :> BININFO = struct

    type complainer = GenericVC.ErrorMsg.complainer
    type region = GenericVC.SourceMap.region

    datatype info =
	INFO of { group: AbsPath.t * region,
		  spec: string,
		  offset: int,
		  share: bool option }

    fun compare (INFO i, INFO i') =
	case Int.compare (#offset i, #offset i') of
	    EQUAL => AbsPath.compare (#1 (#group i), #1 (#group i'))
	  | unequal => unequal

    fun describe (INFO { group = (group, _), spec, offset, ... }) =
	concat [AbsPath.name group, "@", Int.toString offset, "(", spec, ")"]

    fun group (INFO { group = (g, r), ... }) = g
    fun offset (INFO { offset = os, ... }) = os
    fun share (INFO { share = s, ... }) = s

    fun error (gp: GeneralParams.params) (INFO { group, ... }) =
	GroupReg.error (#groupreg gp) group
end

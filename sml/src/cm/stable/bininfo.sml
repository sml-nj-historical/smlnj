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
    val stablePath : info -> AbsPath.t
    val share : info -> bool option
    val error : info -> complainer
end

structure BinInfo :> BININFO = struct

    type complainer = GenericVC.ErrorMsg.complainer

    datatype info =
	INFO of { group: AbsPath.t,
		  stablePath: AbsPath.t,
		  spec: string,
		  offset: int,
		  share: bool option,
		  error: complainer }

    fun compare (INFO i, INFO i') =
	case Int.compare (#offset i, #offset i') of
	    EQUAL => AbsPath.compare (#group i, #group i')
	  | unequal => unequal

    fun describe (INFO { group, spec, offset, ... }) =
	concat [AbsPath.name group, "@", Int.toString offset, "(", spec, ")"]

    fun group (INFO { group = g, ... }) = g
    fun offset (INFO { offset = os, ... }) = os
    fun stablePath (INFO { stablePath = p, ... }) = p
    fun share (INFO { share = s, ... }) = s
    fun error (INFO { error = e, ... }) = e
end

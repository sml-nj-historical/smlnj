(*
 * Building the bootlist from a dependency graph...
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure MkBootList =
    MkListFn (type element = string * int option
	      fun bininfo i = (BinInfo.stablename i, SOME (BinInfo.offset i))
	      fun smlinfo i = (SmlInfo.binname i, NONE))

structure MkBootList =
    MkListFn (type element = AbsPath.t * int option
	      fun bininfo i = (BinInfo.stablepath i, SOME (BinInfo.offset i))
	      fun smlinfo i = (SmlInfo.binpath i, NONE))

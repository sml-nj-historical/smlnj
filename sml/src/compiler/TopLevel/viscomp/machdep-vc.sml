(*
 * Copyright 1998 by Bell Laboratories
 *  machdep-vc.sml -- machine dependent part of viscomp
 *
 * by Matthias Blume (10/1998)
 *)

functor MachDepVCFun (Machm : CODEGENERATOR) : MACHDEP_VC = struct
    structure Interact =
	Interact (EvalLoopF (CompileF (structure M = Machm
				       structure CC = IntConfig)))
    structure Compile = CompileF (structure M = Machm
				  structure CC = BatchConfig)
    structure Binfile = BinfileFun (Compile)
    structure CMSA = CMSAFun (structure BF = Binfile
			      structure C = Compile)
    structure Profile = ProfileFn (ProfEnv (Interact))
    val architecture = Machm.architecture
end


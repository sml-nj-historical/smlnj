(* backend-fn.sml
 * 
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
functor BackendFn (M : CODEGENERATOR) : BACKEND = struct
    structure Interact =
	Interact (EvalLoopF (CompileF (structure M = M
				       structure CC = IntConfig)))
    structure Compile = CompileF (structure M = M
				  structure CC = BatchConfig)
    structure Profile = ProfileFn (ProfEnv (Interact))
    structure Machine = M.Machine
    val architecture = M.architecture
end

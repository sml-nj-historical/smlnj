(* Copyright 1999, Lucent Technologies, Bell Labs *)

structure MLRISCOptimizer = struct
  structure Glue = 
    MLRISCGlue(open Compiler.Machine
	       fun patchBranch{instr,backwards} = [instr]
	       fun branchProb _ = 50)
  val _ = Compiler.Machine.optimizerHook := SOME(Glue.codegen)
end

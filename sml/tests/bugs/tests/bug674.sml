(* bug674.sml *)

val e = Compiler.Environment.filterEnv(#get Compiler.EnvRef.pervasive (),
				       [Compiler.Symbol.varSymbol "hd"]);

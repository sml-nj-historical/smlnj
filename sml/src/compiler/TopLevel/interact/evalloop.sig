(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sig *)
 
signature EVALLOOP =
sig
  exception Interrupt 
  type interactParams = 
       { compManagerHook : (Ast.dec * EnvRef.envref 
                            * EnvRef.envref -> unit) option ref,
         baseEnvRef      : EnvRef.envref,
         localEnvRef     : EnvRef.envref,
         transform       : Absyn.dec -> Absyn.dec,
         instrument      : { source: Source.inputSource,
                             compenv: StaticEnv.staticEnv}
			   -> Absyn.dec -> Absyn.dec,
         perform         : CodeObj.executable -> CodeObj.executable,
         isolate         : CodeObj.executable -> CodeObj.executable,
         printer         : Environment.environment -> PrettyPrint.ppstream 
                           -> (Absyn.dec * Access.lvar list) -> unit}

  val stdParams   : interactParams
  val interact    : interactParams -> unit
  val evalStream  : interactParams -> string * TextIO.instream -> unit

end (* signature EVALLOOP *)

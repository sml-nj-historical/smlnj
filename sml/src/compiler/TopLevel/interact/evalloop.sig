(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sig *)
 
signature EVALLOOP =
sig
  exception Interrupt 
  type interactParams = 
         {compManagerHook : (CompBasic.ast * EnvRef.envref 
                                           * EnvRef.envref -> unit) option ref,
          baseEnvRef      : EnvRef.envref,
          localEnvRef     : EnvRef.envref,
          transform       : CompBasic.absyn -> CompBasic.absyn,
          instrument      : {source: CompBasic.source,
                             compenv: StaticEnv.staticEnv}
                                -> (CompBasic.absyn -> CompBasic.absyn),
          perform         : CompBasic.executable -> CompBasic.executable,
          isolate         : CompBasic.executable -> CompBasic.executable,
          printer         : Environment.environment -> PrettyPrint.ppstream 
                            -> (CompBasic.absyn * Access.lvar list) -> unit}

  val stdParams   : interactParams
  val interact    : interactParams -> unit
  val evalStream  : interactParams -> string * TextIO.instream -> unit

end (* signature EVALLOOP *)



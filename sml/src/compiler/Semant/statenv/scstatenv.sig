(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* scstatenv.sig *)

signature SCSTATICENV =
sig

  type staticEnv
  val empty : staticEnv

  val atop : staticEnv * staticEnv -> staticEnv
  val consolidate : staticEnv -> staticEnv
  val unSC : staticEnv -> StaticEnv.staticEnv
  val SC : StaticEnv.staticEnv -> staticEnv
  val adjSC : staticEnv list * StaticEnv.staticEnv -> staticEnv

  val lookSTR: staticEnv -> ModuleId.modId -> Modules.Structure option
  val lookSIG: staticEnv -> ModuleId.modId -> Modules.Signature option
  val lookFSIG: staticEnv -> ModuleId.modId -> Modules.fctSig option
  val lookFCT: staticEnv -> ModuleId.modId -> Modules.Functor option
  val lookTYC: staticEnv -> ModuleId.modId -> Types.tycon option
  val lookEENV: staticEnv -> ModuleId.modId -> Modules.entityEnv option

  val debugging : bool ref

end (* signature SCSTATICENV *)

(*
 * $Log: scstatenv.sig,v $
 * Revision 1.3  1997/08/11  18:30:22  george
 *   Simplified the modmap handling by no longer paying attention to
 *   space leak problems.  Such problems don't matter in this version,
 *   because modmaps aren't used for the top-level environment.
 * 							-- blume
 *
 * Revision 1.2  1997/02/26  21:50:29  george
 *    Fix the BUG 1116 about very slow top-level interactive loop, reported
 *    by Larry Paulson (Dave, if you have HOL with you, can you test this out?)
 *
 *)

(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cmstatenv.sig *)

signature CMSTATICENV =
sig

  type staticEnv
  val empty : staticEnv

  val atop : staticEnv * staticEnv -> staticEnv
  val consolidate : staticEnv -> staticEnv
  val unCM : staticEnv -> StaticEnv.staticEnv
  val CM : StaticEnv.staticEnv -> staticEnv
  val adjCM : staticEnv list * StaticEnv.staticEnv -> staticEnv

  val lookSTR: staticEnv -> ModuleId.modId -> Modules.Structure option
  val lookSIG: staticEnv -> ModuleId.modId -> Modules.Signature option
  val lookFSIG: staticEnv -> ModuleId.modId -> Modules.fctSig option
  val lookFCT: staticEnv -> ModuleId.modId -> Modules.Functor option
  val lookTYC: staticEnv -> ModuleId.modId -> Types.tycon option
  val lookEENV: staticEnv -> ModuleId.modId -> Modules.entityEnv option

  val debugging : bool ref

end (* signature CMSTATICENV *)

(*
 * $Log: cmstatenv.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:36  george
 * Version 110.5
 *
 *)

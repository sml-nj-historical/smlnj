(* envcvt.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature COERCE_ENV =
 sig val e2b : CMEnv.Env.environment -> Environment.environment
     val es2bs : CMEnv.Env.staticEnv -> Environment.staticEnv
     val b2e : Environment.environment -> CMEnv.Env.environment
     val bs2es : Environment.staticEnv -> CMEnv.Env.staticEnv
     val debugging : bool ref
 end

structure CoerceEnv : COERCE_ENV = 
struct
  val b2e = CMEnv.CM
  val bs2es = CMStaticEnv.CM
  val e2b = CMEnv.unCM
  val es2bs = CMStaticEnv.unCM
  val debugging = CMStaticEnv.debugging
end

(*
 * $Log: envcvt.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)

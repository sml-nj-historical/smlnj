(* envcvt.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature COERCE_ENV =
 sig val e2b : SCEnv.Env.environment -> Environment.environment
     val es2bs : SCEnv.Env.staticEnv -> Environment.staticEnv
     val b2e : Environment.environment -> SCEnv.Env.environment
     val bs2es : Environment.staticEnv -> SCEnv.Env.staticEnv
     val debugging : bool ref
 end

structure CoerceEnv : COERCE_ENV = 
struct
  val b2e = SCEnv.SC
  val bs2es = SCStaticEnv.SC
  val e2b = SCEnv.unSC
  val es2bs = SCStaticEnv.unSC
  val debugging = SCStaticEnv.debugging
end

(*
 * $Log: envcvt.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)

(* scenv.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature SCENV =
sig
  structure Env : ENVIRONMENT 
    where type dynenv = Environment.dynenv
  
  val SC   : Environment.environment -> Env.environment
  val unSC : Env.environment -> Environment.environment
end

(*
 * $Log: scenv.sig,v $
 * Revision 1.2  1997/05/20  12:21:13  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)

(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cmenviron.sig *)


signature CMENV =
sig
  structure Env : ENVIRONMENT 
    where type dynenv = Environment.dynenv
  
  val CM   : Environment.environment -> Env.environment
  val unCM : Env.environment -> Environment.environment
end

(*
 * $Log: cmenviron.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)

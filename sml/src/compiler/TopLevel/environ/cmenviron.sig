(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cmenviron.sig *)


signature CMENV =
sig
  structure Env : ENVIRONMENT 
    where type dynenv = Environment.dynenv
  
  val CM   : Environment.environment -> Env.environment
  val unCM : Env.environment -> Environment.environment
end


(* COPYRIGHT (c) 1996 Bell Laboratories.*)
(* envref.sml *)

signature ENVREF = 
sig
  type staticEnv = StaticEnv.staticEnv
  type environment = Environment.environment

  type senvref = {get: unit -> staticEnv, set: staticEnv -> unit}
  type envref = {get: unit -> environment, set: environment -> unit}

  val topLevel : envref			(* interactive top level env *)
  val pervasive : envref		(* pervasive environment *)
  val combined : unit -> environment
end

structure EnvRef : ENVREF =
struct
  type staticEnv = StaticEnv.staticEnv
  type environment = Environment.environment
  type envref = {get: unit -> environment, set: environment -> unit}
  type senvref = {get: unit->StaticEnv.staticEnv, set: StaticEnv.staticEnv->unit}

  fun mkEnvRef a = 
    let val r = ref a
     in {get=(fn()=> !r),set=(fn x=> r:=x)}
    end

  val topLevel = mkEnvRef Environment.emptyEnv
  val pervasive = mkEnvRef Environment.emptyEnv

  fun combined () = Environment.layerEnv (#get topLevel (), #get pervasive ())
end

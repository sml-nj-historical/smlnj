(* COPYRIGHT (c) 1996 Bell Laboratories.*)
(* envref.sml *)

signature ENVREF = 
sig
  type staticEnv = StaticEnv.staticEnv
  type CMstaticEnv = CMStaticEnv.staticEnv
  type environment = Environment.environment
  type CMenvironment = CMEnv.Env.environment

  type senvref = {get: unit -> staticEnv, set: staticEnv -> unit}
  type envref = {get: unit -> environment, set: environment -> unit}
  type CMenvref = {get: unit -> CMenvironment, set: CMenvironment -> unit}

  val core: senvref
  val topLevel : envref (* interactive top level env *)
  val pervasive : CMenvref (* pervasive environment *)
  val unCM : CMenvref -> envref
  val unCMenv : CMenvironment -> environment
  val unCMstaticEnv : CMstaticEnv -> staticEnv
  val combined : unit -> environment
end

structure EnvRef : ENVREF =
struct
  type staticEnv = StaticEnv.staticEnv
  type CMstaticEnv = CMEnv.Env.staticEnv
  type environment = Environment.environment
  type CMenvironment = CMEnv.Env.environment
  type envref = {get: unit -> environment, set: environment -> unit}
  type CMenvref = {get: unit -> CMenvironment, set: CMenvironment -> unit}
  type senvref = {get: unit->StaticEnv.staticEnv, set: StaticEnv.staticEnv->unit}

  fun mkEnvRef a = 
    let val r = ref a
     in {get=(fn()=> !r),set=(fn x=> r:=x)}
    end

  val core = mkEnvRef StaticEnv.empty 
  val topLevel = mkEnvRef Environment.emptyEnv
  val pervasive = mkEnvRef CMEnv.Env.emptyEnv

  (* set disabled, since it is not expected to be used *)
  fun unCM ({get,set}: CMenvref) : envref = 
      {get = CMEnv.unCM o get,
       set = (fn _ => raise Fail "EnvRef.unCM") (* set o CMEnv.CM *)}

  val unCMenv : CMenvironment -> environment = CMEnv.unCM
  val unCMstaticEnv : CMstaticEnv -> staticEnv = CMStaticEnv.unCM

  fun combined () = 
      Environment.layerEnv(#get topLevel (), CMEnv.unCM(#get pervasive ()))

end


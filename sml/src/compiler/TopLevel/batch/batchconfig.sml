(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* batchconfig.sml *)

structure BatchConfig: CCONFIG = 
struct
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
  type cmstatenv = CMStaticEnv.staticEnv
  val toCM : statenv -> cmstatenv = CMStaticEnv.CM
  val fromCM : cmstatenv -> statenv = CMStaticEnv.unCM
   
  type pickle = Word8Vector.vector
  type hash = PersStamps.persstamp
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val pickUnpick : cmstatenv * statenv ->
                     {hash: hash, pickle: pickle, exportLvars: lvar list,
                      exportPid: pid option, newenv: statenv}
    = fn (compenv, newenv) =>
        let val {hash,pickle,exportLvars,exportPid} = 
              PickMod.pickleEnv { context = compenv, env = newenv }
            val newenv' = 
              UnpickMod.unpickleEnv { context = compenv,
				      hash = hash,
				      pickle = pickle}
         in {hash=hash, pickle=pickle, exportLvars=exportLvars,
             exportPid=exportPid, newenv=newenv'}
        end

  val mkMkStamp : unit -> (unit -> Stamps.stamp) = Stamps.new

end (* structure BatchConfig *)


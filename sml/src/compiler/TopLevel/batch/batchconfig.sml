(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* batchconfig.sml *)

structure BatchConfig: CCONFIG = 
struct
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
  type scstatenv = SCStaticEnv.staticEnv
  val toSC : statenv -> scstatenv = SCStaticEnv.SC
  val fromSC : scstatenv -> statenv = SCStaticEnv.unSC
   
  type pickle = Word8Vector.vector
  type hash = PersStamps.persstamp
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val pickUnpick : scstatenv * statenv ->
                     {hash: hash, pickle: pickle, exportLvars: lvar list,
                      exportPid: pid option, newenv: statenv}
    = fn (compenv, newenv) =>
        let val {hash,pickle,exportLvars,exportPid} = 
              PickMod.pickleEnv(compenv,newenv)
            val newenv' = 
              UnpickMod.unpickleEnv(compenv, {hash=hash,pickle=pickle})
         in {hash=hash, pickle=pickle, exportLvars=exportLvars,
             exportPid=exportPid, newenv=newenv'}
        end

  val makePid : scstatenv * scstatenv -> pid 
    = fn (context, se) => #hash (PickMod.pickleEnv (context, fromSC se))

  val mkMkStamp : unit -> (unit -> Stamps.stamp) = Stamps.new

end (* structure BatchConfig *)
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
              PickMod.pickleEnv(compenv,newenv)
            val newenv' = 
              UnpickMod.unpickleEnv(compenv, {hash=hash,pickle=pickle})
         in {hash=hash, pickle=pickle, exportLvars=exportLvars,
             exportPid=exportPid, newenv=newenv'}
        end

  val makePid : cmstatenv * cmstatenv -> pid 
    = fn (context, se) => #hash (PickMod.pickleEnv (context, fromCM se))

  val mkMkStamp : unit -> (unit -> Stamps.stamp) = Stamps.new

end (* structure BatchConfig *)

(*
 * $Log: batchconfig.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)

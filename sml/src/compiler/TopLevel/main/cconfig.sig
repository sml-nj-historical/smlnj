(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cconfig.sml *)

signature CCONFIG = 
sig
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
  type cmstatenv
  val toCM : statenv -> cmstatenv
  val fromCM : cmstatenv -> statenv
   
  type pickle
  type hash
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val pickUnpick : cmstatenv * statenv ->
                     {hash: hash, pickle: pickle, exportLvars: lvar list,
                      exportPid: pid option, newenv: statenv}

  val makePid : cmstatenv * cmstatenv -> pid
  val mkMkStamp : unit -> (unit -> Stamps.stamp)

end (* signature CCONFIG *)

(*
 * $Log$
 *)

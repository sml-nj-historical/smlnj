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

  val mkMkStamp : unit -> (unit -> Stamps.stamp)

end (* signature CCONFIG *)

(*
 * $Log: cconfig.sig,v $
 * Revision 1.2  1998/10/16 14:03:58  george
 *   Implemented a hierachical bin directory structure and
 *   broke up the Compiler structure into a machine dependent
 *   and independent parts. [blume]
 *
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)

(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs *)

(* Signature to capture various aspects of the MLRISC back end *) 

signature MACHINE = sig
  structure F : FLOWGRAPH
  structure P : INSN_PROPERTIES
	where I = F.I and C = F.C
  structure Asm : INSTRUCTION_EMITTER
        where I = F.I and  P = F.P 

  type mlriscPhase = string * (F.cluster -> F.cluster) 
  val makePhase     : string * (F.cluster -> F.cluster) -> mlriscPhase
  val raPhase       : mlriscPhase 
  val optimizerHook : mlriscPhase list ref
  val finish        : unit -> unit
end (* MACHINE *)



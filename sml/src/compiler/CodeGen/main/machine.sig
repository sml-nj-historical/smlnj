(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs *)

(* Signature to capture various aspects of the MLRISC back end *) 

signature MACHINE = sig
  structure F : FLOWGRAPH
  structure P : INSN_PROPERTIES
	where I = F.I and C = F.C
  structure Asm : INSTRUCTION_EMITTER
        where I = F.I and  P = F.P 

  val copyProp :  F.cluster -> F.cluster

  val optimizerHook : (F.cluster -> F.cluster) option ref

  val finish : unit -> unit
end (* MACHINE *)



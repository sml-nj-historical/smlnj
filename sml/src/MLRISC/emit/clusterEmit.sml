(* cfgEmit.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * This module takes a flowgraph and an assembly emitter module and 
 * ties them together into one.  The output is sent to AsmStream.
 *  --Allen
 *)

functor CFGEmit
  (structure CFG : CONTROL_FLOW_GRAPH
   structure E : INSTRUCTION_EMITTER
      where I = CFG.I and P = CFG.P) : ASSEMBLY_EMITTER = 
struct

  structure CFG = CFG

  fun asmEmit (Graph.GRAPH graph, blocks) = let
	val CFG.INFO{annotations=an, ...} = #graph_info graph
	val E.S.STREAM{pseudoOp,defineLabel,emit,annotation,comment,...} = 
             E.makeStream (!an)
	fun emitAn a = if Annotations.toString a = "" then () else annotation(a)
	fun emitData (CFG.LABEL lab) = defineLabel lab
	  | emitData (CFG.PSEUDO pOp) = pseudoOp pOp
	fun emitIt (id, CFG.BLOCK{data, labels, annotations=a, insns, ...}) = (
	      List.app emitData (!data);
(*	      List.app defineLabel (!labels); *) (* JHR *)
	      List.app emitAn (!a);
	      List.app emit (rev (!insns)))
	in
	  List.app emitAn (!an);
	  List.app emitIt blocks
	end
end


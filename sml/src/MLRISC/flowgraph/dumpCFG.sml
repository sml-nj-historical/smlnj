signature DUMP_CFG = sig
  structure CFG : CONTROL_FLOW_GRAPH

  val dumpCFG : TextIO.outstream -> string *  CFG.cfg -> unit
end



functor DumpCFG
  ( structure CFG : CONTROL_FLOW_GRAPH
    structure Asm : INSTRUCTION_EMITTER
		where P=CFG.P
		  and I = CFG.I
   ) : DUMP_CFG = 
struct

  val Asm.S.STREAM{emit, pseudoOp, comment, ...} = Asm.makeStream []

  structure CFG = CFG

  fun dumpCFG outStrm (text, cfg as Graph.GRAPH graph) = let
    fun dumpData 
    fun dumpNode(nid, CFG.BLOCK{data, insns, ...}) = 
      (app dumpData (!data); app emit insns)
      

  in
    comment text;
    AsmStream.withStream outStrm 
      (fn () => (#forall_nodes graph dumpNode)) ()
  end
end
(* printFlowgraph.sml -- print flowgraph of target machine instructions. 
 *
 * Copyright (c) 1997 Bell Laboratories.
 *)
signature PRINT_FLOWGRAPH = 
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure Asm : INSTRUCTION_EMITTER
      where I = CFG.I and P = CFG.P

   val printCFG : TextIO.outstream -> string -> CFG.cfg -> unit
end


functor PrintFlowgraph 
   (structure CFG : CONTROL_FLOW_GRAPH
    structure Asm : INSTRUCTION_EMITTER 
       where P=CFG.P and I=CFG.I
   ) : PRINT_FLOWGRAPH =
struct
   structure Asm = Asm
   structure CFG = CFG
   structure C   = CFG.I.C
   structure W   = CFG.W

   val i2s = Int.toString

   fun printList stream list = let
     fun pr str = TextIO.output(stream, str)
     fun iter [] = ()
       | iter [i] = pr i
       | iter (h::t) = (pr (h ^ ", "); iter t)
   in iter list
   end

   fun printCFG stream title (Cfg as Graph.GRAPH cfg) = 
   let fun pr str = TextIO.output(stream, str)
       val prList = printList stream
       val annotations = !(CFG.annotations Cfg)
       val Asm.S.STREAM{emit,pseudoOp,defineLabel,annotation,...} = 
             AsmStream.withStream stream Asm.makeStream annotations

       fun showFreq(ref w) = "["^W.toString w^"]"
       fun showEdge(blknum,e) = i2s blknum^":"^CFG.show_edge e
       fun showSucc(_, x, e) = showEdge(x,e)
       fun showPred(x, _, e) = showEdge(x,e)
       fun showSuccs b =
            (pr "\tsucc:     "; 
             prList (map showSucc (#out_edges cfg b)); 
             pr "\n")
       fun showPreds b =
            (pr "\tpred:     "; 
             prList (map showPred (#in_edges cfg b)); 
             pr "\n")

       fun printBlock(_, CFG.BLOCK{kind=CFG.START, id, freq, ...}) = 
           (pr ("ENTRY " ^ i2s id ^ showFreq freq^"\n");
            showSuccs id)
         | printBlock(_, CFG.BLOCK{kind=CFG.STOP, id, freq, ...}) = 
           (pr ("EXIT " ^ i2s id ^ showFreq freq ^"\n");
            showPreds id)
         | printBlock(_, CFG.BLOCK{id, freq, insns, annotations, data, 
                               labels, ...}) = 
           (pr ("BLOCK " ^ i2s id ^ showFreq freq ^ "\n");
            app annotation (!annotations);
            app (fn CFG.PSEUDO pOp => pseudoOp pOp
                  | CFG.LABEL l    => defineLabel l
                ) (!data);
            app defineLabel (!labels);
            (*pr ("\tlive in:  " ^ CellsBasis.CellSet.toString (!liveIn) ^ "\n");
            pr ("\tlive out: " ^ CellsBasis.CellSet.toString (!liveOut) ^ "\n");*)
            showSuccs id;
            showPreds id;
            app emit (rev (!insns)))
   in
       pr("[ "^ title ^" ]\n");
       app annotation annotations;
       (* printBlock entry; *)
       AsmStream.withStream stream (#forall_nodes cfg) printBlock;
       (* printBlock exit; *)
       TextIO.flushOut stream
   end
end


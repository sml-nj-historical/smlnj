(* printFlowgraph.sml -- print flowgraph of target machine instructions. 
 *
 * Copyright (c) 1997 Bell Laboratories.
 *)
signature PRINT_CLUSTER = 
sig
   structure F : FLOWGRAPH
   structure E : INSTRUCTION_EMITTER
      where I = F.I and P = F.P

   val printCluster : TextIO.outstream -> string -> F.cluster -> unit
end


functor PrintClusterFn 
   (structure Flowgraph : FLOWGRAPH
    structure Asm       : INSTRUCTION_EMITTER 
       where P=Flowgraph.P and I=Flowgraph.I
   ) : PRINT_CLUSTER =
struct
   structure E = Asm
   structure F = Flowgraph
   structure C = Flowgraph.C
   structure W = F.W

   fun printList stream list = let
     fun pr str = TextIO.output(stream, str)
     fun iter [] = ()
       | iter [i] = pr i
       | iter (h::t) = (pr (h ^ ", "); iter t)
   in iter list
   end

   fun showFreq(ref w) = "["^W.toString w^"]"
   fun showEdge'(blknum,w) = Int.toString blknum^showFreq w
   fun showEdge(F.BBLOCK{blknum, ...},w) = showEdge'(blknum,w)
     | showEdge(F.ENTRY{blknum, ...},w) = showEdge'(blknum,w)
     | showEdge(F.EXIT{blknum, ...},w) = showEdge'(blknum,w)

   fun printCluster stream title 
        (F.CLUSTER {blocks, regmap, entry, exit, annotations, ...}) = 
   let fun pr str = TextIO.output(stream, str)
       val prList = printList stream
       val regmap = C.lookup regmap
       val E.S.STREAM{emit,pseudoOp,defineLabel,annotation,...} = 
             AsmStream.withStream stream E.makeStream()
       val emit = emit regmap

       fun printEntry(F.ENTRY{blknum, succ, freq, ...}) = 
         (pr ("ENTRY " ^ Int.toString blknum ^ showFreq freq^"\n");
          pr "\tsucc:     "; prList (map showEdge (!succ));	pr "\n")

       fun printBlock(F.PSEUDO pOp) = pseudoOp pOp
         | printBlock(F.LABEL l)    = defineLabel l
         | printBlock(F.BBLOCK{blknum, freq, succ, pred, liveOut, liveIn, 
                               insns, name, annotations, ...}) = 
          (pr ("BLOCK " ^ Int.toString blknum  
                        ^ showFreq freq ^ "(" ^F.B.toString name ^ ")\n");
           app annotation (!annotations);
           pr ("\tlive in:  " ^ C.cellsetToString' regmap (!liveIn) ^ "\n");
	   pr ("\tlive out: " ^ C.cellsetToString' regmap (!liveOut) ^ "\n");
	   pr ("\tsucc:     "); prList (map showEdge (!succ)); pr "\n";
	   pr ("\tpred:     "); prList (map showEdge (!pred)); pr "\n";
	   app emit (rev (!insns)))

       fun printExit(F.EXIT{blknum, pred, freq, ...}) = 
         (pr ("EXIT " ^ Int.toString blknum ^ showFreq freq ^"\n");
          pr "\tpred      "; prList (map showEdge (!pred));  pr "\n")
   in
       pr("[ "^ title ^" ]\n");
       app annotation (!annotations);
       printEntry entry;
       AsmStream.withStream stream (app printBlock) blocks;
       printExit exit;
       TextIO.flushOut stream
   end
end


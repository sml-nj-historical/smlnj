(*
 * This module performs static branch prediction using heuristics
 * similar to Ball and Larus'
 *
 * -- Allen
 *)

functor StaticBranchPredictionFn
   (structure IR : MLRISC_IR
    structure Props : INSN_PROPERTIES
    structure FreqProps : FREQUENCY_PROPERTIES
       sharing IR.I = FreqProps.I = Props.I
   ) : STATIC_BRANCH_PREDICTION =
struct

   structure IR   = IR
   structure CFG  = IR.CFG
   structure CompFreq = ComputeFrequenciesFn(structure Loop = IR.Loop
                                             structure Freq = CFG.W)

   fun profile {loopMultiplier} IR =
   let fun branchProb(CFG.BLOCK{insns,...}) = 
           case !insns of
             [] => 100 (* the fallsthru edge is always taken *)
           | jmp::_ => 
              (case Props.instrKind jmp of 
                 Props.IK_JUMP => FreqProps.branchProb jmp
               | _ => 100 (* the fallsthru edge is always taken *)
              )
                  

       fun nodeFreq(CFG.BLOCK{freq,...}) = freq
       fun edgeFreq(CFG.EDGE{w,...}) = w

       (* is this a conditional branch and is it in the taken direction? *)
       fun isTakenBranch(_,_,CFG.EDGE{k=CFG.BRANCH b,...}) = b
         | isTakenBranch _ = false

   in  CompFreq.compute_frequencies
       { cfg            = IR,
         loop           = IR.loop IR,
         loopMultiplier = loopMultiplier,
         nodeFreq       = nodeFreq,
         edgeFreq       = edgeFreq,
         branchProb     = branchProb,
         isTakenBranch  = isTakenBranch
       } 
   end

end

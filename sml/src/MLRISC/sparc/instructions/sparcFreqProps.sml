(*
 * Extract frequency information from the sparc architecture
 * 
 * -- Allen
 *)

functor SparcFreqProps(SparcInstr : SPARCINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = SparcInstr

   fun cond I.BA  = 100
     | cond I.BE  = 10
     | cond I.BNE = 90
     | cond _     = 50

   fun fcond I.FBA  = 100
     | fcond I.FBE  = 10
     | fcond I.FBNE = 90
     | fcond _      = 50

   fun sparcBranchProb(I.Bicc{b,...}) = cond b
     | sparcBranchProb(I.FBfcc{b,...}) = fcond b
     | sparcBranchProb(I.BP{b,...}) = cond b
     | sparcBranchProb(I.BR _) = 50
     | sparcBranchProb(I.JMP _) = 100
     | sparcBranchProb(I.RET _) = 100
     | sparcBranchProb _ = 0 (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR(i)) = Probability.percent (sparcBranchProb i)
     | branchProb _ = Probability.never

end

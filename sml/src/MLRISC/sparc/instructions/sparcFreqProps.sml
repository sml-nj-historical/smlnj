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

   fun branchProb(I.ANNOTATION{a=BasicAnnotations.BRANCH_PROB b,...}) = b
     | branchProb(I.ANNOTATION{i,...}) = branchProb i
     | branchProb(I.Bicc{b,...}) = cond b
     | branchProb(I.FBfcc{b,...}) = fcond b
     | branchProb(I.BP{b,...}) = cond b
     | branchProb(I.BR _) = 50
     | branchProb(I.JMP _) = 100
     | branchProb(I.RET _) = 100
     | branchProb _ = 0 (* non-branch *)

end

(*
 * Extract frequency information from the PowerPC architecture
 *
 * -- Allen
 *)

functor PPCFreqProps(PPCInstr : PPCINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = PPCInstr

   fun ppcBranchProb(I.BC _) = 50
     | ppcBranchProb(I.BCLR{labels=[],bo=I.ALWAYS,...}) = 100
     | ppcBranchProb(I.BCLR{labels,bo=I.ALWAYS,...}) = 100 div length labels
     | ppcBranchProb(I.BCLR{labels=[],bo,...}) = 50
     | ppcBranchProb(I.BCLR{labels,bo,...}) = 100 div length labels
     | ppcBranchProb _ = 0 (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR(i)) = ppcBranchProb(i)
     | branchProb _ = 0

end

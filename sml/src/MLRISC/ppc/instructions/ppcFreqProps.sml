(*
 * Extract frequency information from the PowerPC architecture
 *
 * -- Allen
 *)

functor PPCFreqProps(PPCInstr : PPCINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = PPCInstr

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek BasicAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.BC _) = 50
     | branchProb(I.BCLR{labels=[],bo=I.ALWAYS,...}) = 100
     | branchProb(I.BCLR{labels,bo=I.ALWAYS,...}) = 100 div length labels
     | branchProb(I.BCLR{labels=[],bo,...}) = 50
     | branchProb(I.BCLR{labels,bo,...}) = 100 div length labels
     | branchProb _ = 0 (* non-branch *)

end

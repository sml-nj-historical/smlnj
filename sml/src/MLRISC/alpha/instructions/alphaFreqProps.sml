(*
 * Extract frequency information from the Alpha architecture
 * 
 * -- Allen
 *)

functor AlphaFreqProps(AlphaInstr : ALPHAINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = AlphaInstr

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.BRANCH{b=I.BR, ...}) = 100 (* unconditional *)
     | branchProb(I.BRANCH{b=I.BEQ, ...}) = 10
     | branchProb(I.BRANCH{b=I.BNE, ...}) = 90
     | branchProb(I.FBRANCH{b=I.FBEQ, ...}) = 10 
     | branchProb(I.FBRANCH{b=I.FBNE, ...}) = 90
     | branchProb(I.BRANCH _) = 50 (* default *)
     | branchProb(I.FBRANCH _) = 50 (* default *)
     | branchProb(I.JMPL(_,[])) = 100 (* unconditional *)
     | branchProb(I.JMPL(_,labs)) = 100 div length labs  (* assume equal prob *)
     | branchProb(I.RET _) = 100
     | branchProb _ = 0 (* non-branch *)

end

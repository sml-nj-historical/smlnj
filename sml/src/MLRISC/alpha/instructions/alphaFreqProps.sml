(*
 * Extract frequency information from the Alpha architecture
 * 
 * -- Allen
 *)

functor AlphaFreqProps(AlphaInstr : ALPHAINSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = AlphaInstr

   fun alphaBranchProb(I.BRANCH{b=I.BR, ...}) = 100 (* unconditional *)
     | alphaBranchProb(I.BRANCH{b=I.BEQ, ...}) = 10
     | alphaBranchProb(I.BRANCH{b=I.BNE, ...}) = 90
     | alphaBranchProb(I.FBRANCH{b=I.FBEQ, ...}) = 10 
     | alphaBranchProb(I.FBRANCH{b=I.FBNE, ...}) = 90
     | alphaBranchProb(I.BRANCH _) = 50 (* default *)
     | alphaBranchProb(I.FBRANCH _) = 50 (* default *)
     | alphaBranchProb(I.JMPL(_,[])) = 100 (* unconditional *)
     | alphaBranchProb(I.JMPL(_,labs)) = 100 div length labs  (* assume equal prob *)
     | alphaBranchProb(I.RET _) = 100
     | alphaBranchProb _ = 0 (* non-branch *)

   fun branchProb(I.ANNOTATION{a, i, ...}) = 
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.INSTR i) = Probability.percent (alphaBranchProb i)
     | branchProb _ = Probability.never			(* non-branch *)
end

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
     | branchProb(I.BRANCH(I.BR,_,_)) = 100 (* unconditional *)
     | branchProb(I.BRANCH(I.BEQ,_,_)) = 10
     | branchProb(I.BRANCH(I.BNE,_,_)) = 90
     | branchProb(I.FBRANCH(I.FBEQ,_,_)) = 10 
     | branchProb(I.FBRANCH(I.FBNE,_,_)) = 90
     | branchProb(I.BRANCH _) = 50 (* default *)
     | branchProb(I.FBRANCH _) = 50 (* default *)
     | branchProb(I.JMPL(_,[])) = 100 (* unconditional *)
     | branchProb(I.JMPL(_,labs)) = 100 div length labs  (* assume equal prob *)
     | branchProb(I.RET _) = 100
     | branchProb _ = 0 (* non-branch *)

end

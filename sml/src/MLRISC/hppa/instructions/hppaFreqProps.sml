(*
 * Extract frequency properties from the HP architecture
 * 
 * -- Allen
 *)

functor HppaFreqProps(HppaInstr : HPPAINSTR): FREQUENCY_PROPERTIES =
struct

   structure I = HppaInstr

   fun hppaBranchProb(I.BCOND{cmp=I.COMBT,bc=I.EQ,...}) = 10
     | hppaBranchProb(I.BCOND{cmp=I.COMBF,bc=I.EQ,...}) = 90
     | hppaBranchProb(I.BCOND{cmp=I.COMBT,bc=I.NE,...}) = 90
     | hppaBranchProb(I.BCOND{cmp=I.COMBF,bc=I.NE,...}) = 10
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.EQ,...}) = 10
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.EQ,...}) = 90
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.NE,...}) = 90
     | hppaBranchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.NE,...}) = 10
     | hppaBranchProb(I.BCOND _) = 50 (* default *)
     | hppaBranchProb(I.BCONDI _) = 50 (* default *)
     | hppaBranchProb(I.FBRANCH _) = 50 (* default *)
     (*| hppaBranchProb(I.BB{bc=I.BCLR, p=31, ...}) = 10 
     | hppaBranchProb(I.BB{bc=I.BSET, p=31, ...}) = 90 *)
     | hppaBranchProb(I.BB _) = 50 (* branch on bit *)
     | hppaBranchProb(I.B _) = 100 (* unconditional *)
     | hppaBranchProb(I.BE{labs=[], ...}) = 100 (* escapes *)
     | hppaBranchProb(I.BE{labs,...}) = 100 div length labs (* assume equal prob *)
     | hppaBranchProb(I.BV{labs=[],...}) = 100 (* escapes *)
     | hppaBranchProb(I.BV{labs,...}) = 100 div length labs (* assume equal prob *)
     | hppaBranchProb(I.BLR{labs,...}) = 100 div length labs (* assume equal prob *)
     | hppaBranchProb _ = 0 (* non-branch *)
   fun branchProb(I.ANNOTATION{a, i, ...}) =
         (case #peek MLRiscAnnotations.BRANCH_PROB a of
            SOME b => b 
          | NONE => branchProb i
         )
     | branchProb(I.INSTR(i)) = hppaBranchProb(i)
     | branchProb _ = 0

end

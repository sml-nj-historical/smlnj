(*
 * Extract frequency properties from the HP architecture
 * 
 * -- Allen
 *)

functor HppaFreqProps(HppaInstr : HPPAINSTR): FREQUENCY_PROPERTIES =
struct

   structure I = HppaInstr

   fun branchProb(I.ANNOTATION{a, i, ...}) =
         (case #peek BasicAnnotations.BRANCH_PROB a of
            SOME b => b 
          | NONE => branchProb i
         )
     | branchProb(I.BCOND{cmp=I.COMBT,bc=I.EQ,...}) = 10
     | branchProb(I.BCOND{cmp=I.COMBF,bc=I.EQ,...}) = 90
     | branchProb(I.BCOND{cmp=I.COMBT,bc=I.NE,...}) = 90
     | branchProb(I.BCOND{cmp=I.COMBF,bc=I.NE,...}) = 10
     | branchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.EQ,...}) = 10
     | branchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.EQ,...}) = 90
     | branchProb(I.BCONDI{cmpi=I.COMIBT,bc=I.NE,...}) = 90
     | branchProb(I.BCONDI{cmpi=I.COMIBF,bc=I.NE,...}) = 10
     | branchProb(I.BCOND _) = 50 (* default *)
     | branchProb(I.BCONDI _) = 50 (* default *)
     | branchProb(I.FBRANCH _) = 50 (* default *)
     | branchProb(I.BB _) = 50 (* branch on bit *)
     | branchProb(I.B _) = 100 (* unconditional *)
     | branchProb(I.BE{labs=[], ...}) = 100 (* escapes *)
     | branchProb(I.BE{labs,...}) = 100 div length labs (* assume equal prob *)
     | branchProb(I.BV{labs=[],...}) = 100 (* escapes *)
     | branchProb(I.BV{labs,...}) = 100 div length labs (* assume equal prob *)
     | branchProb(I.BLR{labs,...}) = 100 div length labs (* assume equal prob *)
     | branchProb _ = 0 (* non-branch *)

end

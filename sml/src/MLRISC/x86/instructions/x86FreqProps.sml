(*
 * Extract frequency information from the X86 architecture
 *
 * -- Allen
 *)
functor X86FreqProps(X86Instr : X86INSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = X86Instr

   fun x86BranchProb(I.JCC{cond=I.EQ,...}) = 10
     | x86BranchProb(I.JCC{cond=I.O,...}) = 0 (* overflow *)
     | x86BranchProb(I.JCC{cond=I.NE,...}) = 90
     | x86BranchProb(I.JCC{cond=I.NO,...}) = 100
     | x86BranchProb(I.JCC _) = 50 (* default *)
     | x86BranchProb(I.JMP _) = 100 
     | x86BranchProb _ = 0 (* non-branch *)

   and branchProb(I.ANNOTATION{a, i, ...}) = 
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb (I.INSTR i) = x86BranchProb i
     | branchProb _ = 0
end


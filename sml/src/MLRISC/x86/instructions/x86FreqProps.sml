(*
 * Extract frequency information from the X86 architecture
 *
 * -- Allen
 *)
functor X86FreqProps(X86Instr : X86INSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = X86Instr

   fun branchProb(I.ANNOTATION{a, i, ...}) =
        (case #peek BasicAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb(I.JCC{cond=I.EQ,...}) = 10
     | branchProb(I.JCC{cond=I.O,...}) = 0 (* overflow *)
     | branchProb(I.JCC{cond=I.NE,...}) = 90
     | branchProb(I.JCC{cond=I.NO,...}) = 100
     | branchProb(I.JCC _) = 50 (* default *)
     | branchProb(I.JMP _) = 100 
     | branchProb _ = 0 (* non-branch *)

end


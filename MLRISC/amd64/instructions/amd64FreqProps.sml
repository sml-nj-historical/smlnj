(* amd64FreqProps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Extract frequency information from the AMD64 architecture
 *
 * -- Allen
 *)
functor AMD64FreqProps(AMD64Instr : AMD64INSTR) : FREQUENCY_PROPERTIES =
struct

   structure I = AMD64Instr

   val p0_001 = Probability.prob(1,1000)
   val p10 = Probability.percent 10
   val p50 = Probability.percent 50
   val p90 = Probability.percent 90
   val p100 = Probability.always

   fun amd64BranchProb(I.JCC{cond=I.EQ,...}) = p10
     | amd64BranchProb(I.JCC{cond=I.O,...}) = p0_001
     | amd64BranchProb(I.JCC{cond=I.NE,...}) = p90
     | amd64BranchProb(I.JCC{cond=I.NO,...}) = p100
     | amd64BranchProb(I.JCC{cond=I.P,...}) = p0_001 (* fp unordered test *)
     | amd64BranchProb(I.JCC{cond=I.NP,...}) =  p100
     | amd64BranchProb(I.JCC _) = p50 (* default *)
     | amd64BranchProb(I.JMP _) = p100 
     | amd64BranchProb _ = Probability.never (* non-branch *)

   and branchProb(I.ANNOTATION{a, i, ...}) = 
        (case #peek MLRiscAnnotations.BRANCH_PROB a of
           SOME b => b
         | NONE => branchProb i
        )
     | branchProb (I.INSTR i) = amd64BranchProb i
     | branchProb _ = Probability.never

end


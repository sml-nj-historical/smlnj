(* This is a simple deBruijn indexing test with curried functors. 
   I.instruction should be given a deBruijn index of 1 not 2. 
 *)
signature RAG = 
sig
  datatype spillLoc = A
end

functor RegisterAllocator
   (SpillHeuristics : 
    sig
	structure G : RAG
    end)
   (Flowgraph :
    sig
	structure Spill :
sig

   structure I : sig type instruction end
   structure G : RAG

   val spillRewrite : I.instruction -> G.spillLoc
 
end
    end
)  =
struct end

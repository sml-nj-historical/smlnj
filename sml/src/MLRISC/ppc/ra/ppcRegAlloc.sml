functor PPCRegAlloc(structure I : INSTRUCTIONS where C = PPCCells
		     structure P : INSN_PROPERTIES where I = I
		     structure F : FLOWGRAPH where I = I and P = P
		     structure Asm : EMITTER_NEW where I = I and P = P) :
  sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : RA
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : RA
  end=
struct

  structure C=I.C

  (* liveness analysis for general purpose registers *)
  structure RegLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:PPCCells.cellset)
	     fun cellset((_,f,c),r) = (r,f,c))


  functor IntRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=RegLiveness

	   val defUse = P.defUse C.GP
	   val firstPseudoR = 32
	   val maxPseudoR = PPCCells.maxCell
	   val numRegs = PPCCells.numCell PPCCells.GP
	   fun regSet c = #1 (c:PPCCells.cellset)
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:PPCCells.cellset)
	     fun cellset((r,_,c),f) = (r,f,c))

  functor FloatRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=FregLiveness

 	   val defUse = P.defUse C.FP
	   val firstPseudoR = 32
	   val maxPseudoR = PPCCells.maxCell 
	   val numRegs = PPCCells.numCell PPCCells.FP
	   fun regSet c = #2 (c:PPCCells.cellset)
	end)
end


functor X86RegAlloc
    (structure I : INSTRUCTIONS where C = X86Cells
     structure P : INSN_PROPERTIES where I = I
     structure F : FLOWGRAPH where I = I
     structure Asm : EMITTER_NEW where I = I and P = F.P
    ) : 
  sig

    functor IntRa (structure RaUser : RA_USER_PARAMS 
		     where type I.operand = I.operand
		     and type I.instruction = I.instruction
		     and type B.name = F.B.name) : RA

    functor FloatRa (structure RaUser : RA_USER_PARAMS 
		     where type I.operand = I.operand
		     and type I.instruction = I.instruction
		     and type B.name = F.B.name) : RA
  end =
struct
  structure C = I.C

    (* liveness analysis for general purpose registers *)
  structure RegLiveness =
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:X86Cells.cellset)
	     fun cellset((_,f,c),r) = (r,f,c))


  (* integer register allocator *)
  functor IntRa = 
      RegAllocator
	 (structure RaArch = struct

	     structure InsnProps = P
	     structure AsmEmitter = Asm
	     structure I = I
	     structure Liveness=RegLiveness
	     val defUse = P.defUse C.GP
	     val firstPseudoR = 32
	     val maxPseudoR = X86Cells.maxCell
	     val numRegs = X86Cells.numCell C.GP
	     val unusedReg = ~1
	     fun regSet c = #1 (c:X86Cells.cellset)
	  end)



  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:X86Cells.cellset)
	     fun cellset((r,_,c),f) = (r,f,c))

  (* floating register allocator *)
  functor FloatRa = 
    RegAllocator
       (structure RaArch = struct

          structure InsnProps = P
	  structure AsmEmitter = Asm
	  structure Liveness=FregLiveness
	  structure I = I

	  val defUse = P.defUse C.FP
	  val firstPseudoR = 32
	  val maxPseudoR = X86Cells.maxCell
	  val numRegs = X86Cells.numCell C.FP
	  val unusedReg = ~1
	  fun regSet c = #2 (c:X86Cells.cellset)
	end)
end

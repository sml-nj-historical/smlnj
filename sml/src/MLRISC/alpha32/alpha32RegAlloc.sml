(* alpha32RegAlloc.sml --- alpha integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)



functor Alpha32RegAlloc(structure P : INSN_PROPERTIES
			structure F : FLOWGRAPH
			structure I : INSTRUCTIONS where C = Alpha32Cells
			structure Asm : EMITTER_NEW
			   sharing Asm.F = F
		  	   sharing P.I = F.I = Asm.I = I) :
   sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction
		     (* should be: where I = I -- bug 1205 *)) : sig
        datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
	val ra : mode -> F.cluster -> F.cluster
      end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		       where type I.operand = I.operand
		         and type I.instruction = I.instruction
		       (* should be: where I = I *)) : sig
        datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
        val ra : mode -> F.cluster -> F.cluster
      end
   end =
struct
  structure C = I.C
    (* liveness analysis for general purpose registers *)
  structure RegLiveness =
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:Alpha32Cells.cellset)
	     fun cellset((_,f),r) = (r,f))


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
	     val maxPseudoR = Alpha32Cells.maxCell
	     val numRegs = Alpha32Cells.numCell Alpha32Cells.GP
	     fun regSet c = #1 (c:Alpha32Cells.cellset)
	  end)



  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:Alpha32Cells.cellset)
	     fun cellset((r,_),f) = (r,f))

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
	  val maxPseudoR = Alpha32Cells.maxCell 
	  val numRegs = Alpha32Cells.numCell Alpha32Cells.FP
	  fun regSet c = #2 (c:Alpha32Cells.cellset)
	end)
end




(*
 * $Log: alpha32RegAlloc.sml,v $
 * Revision 1.2  1998/05/19 15:43:26  george
 *   The instructions properties now exports a generic defUse function that is
 *   curried over the cellclass, i.e., defUseR and defUseF are gone.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)

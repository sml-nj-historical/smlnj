(* alpha32RegAlloc.sml --- alpha integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)



functor Alpha32RegAlloc(structure I : INSTRUCTIONS where C = Alpha32Cells
			structure P : INSN_PROPERTIES where I = I
			structure F : FLOWGRAPH where I = I 
			structure Asm : EMITTER_NEW 
			  where I = I and P = F.P) :
   sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : RA 

    functor FloatRa (structure RaUser : RA_USER_PARAMS
		       where I = I
		       where type B.name = F.B.name) : RA
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
 * Revision 1.1.1.1  1999/01/04 21:55:01  george
 *   Version 110.12
 *
 * Revision 1.6  1998/10/06 14:07:31  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.5  1998/09/30 19:34:39  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.4  1998/07/25 03:08:13  george
 *   added to support block names in MLRISC
 *
 * Revision 1.3  1998/05/25 15:10:49  george
 *   Fixed RCS keywords
 *
 *)

(* hppaRegAlloc.sml --- hppa integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)

functor HppaRegAlloc(structure P : INSN_PROPERTIES
		     structure F : FLOWGRAPH 
		     structure I : INSTRUCTIONS where C = HppaCells
		     structure Asm : EMITTER_NEW
		         sharing Asm.F = F
			 sharing P.I = F.I = Asm.I = I) :
  sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
   end=
struct

  (* liveness analysis for general purpose registers *)
  structure RegLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUseR
	     fun regSet c = #1 (c:HppaCells.cellset)
	     fun cellset((_,f),r) = (r,f))


  functor IntRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=RegLiveness

	   val defUse = P.defUseR
	   val firstPseudoR = 32
	   val maxPseudoR = HppaCells.maxReg
	   val numRegs = HppaCells.numRegs
	   fun regSet c = #1 (c:HppaCells.cellset)
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUseF
	     fun regSet c = #2 (c:HppaCells.cellset)
	     fun cellset((r,_),f) = (r,f))

  functor FloatRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=FregLiveness

 	   val defUse = P.defUseF
	   val firstPseudoR = 32
	   val maxPseudoR = HppaCells.maxFreg
	   val numRegs = HppaCells.numFregs
	   fun regSet c = #2 (c:HppaCells.cellset)
	end)
end

(*
 * $Log: hppaRegAlloc.sml,v $
 * Revision 1.6  1997/07/17 12:28:02  george
 *   The regmap is now represented as an int map rather than using arrays.
 *
# Revision 1.5  1997/07/15  15:44:05  dbm
#   Change in where structure syntax.
#
# Revision 1.4  1997/07/02  13:22:17  george
#   The register allocator now has a mode to do either copy-propagation
#   or full register allocation + copy-propagation.
#
# Revision 1.3  1997/05/22  03:24:38  dbm
#   Minor cleanup in SML '97 specs.  Still can't use "where structure"
#   because of bug 1205.
#
# Revision 1.1.1.1  1997/04/19  18:14:23  george
#   Version 109.27
#
 *)

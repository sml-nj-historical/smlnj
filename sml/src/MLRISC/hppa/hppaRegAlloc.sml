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
		       and type I.instruction = I.instruction
		       and type B.name = F.B.name) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where type I.operand = I.operand
		       and type I.instruction = I.instruction
		       and type B.name = F.B.name) : sig
      datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION
      val ra : mode -> F.cluster -> F.cluster
     end
   end=
struct

  structure C=I.C

  (* liveness analysis for general purpose registers *)
  structure RegLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     fun regSet c = #1 (c:HppaCells.cellset)
	     fun cellset((_,f),r) = (r,f))


  functor IntRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=RegLiveness

	   val defUse = P.defUse C.GP
	   val firstPseudoR = 32
	   val maxPseudoR = HppaCells.maxCell
	   val numRegs = HppaCells.numCell HppaCells.GP
	   fun regSet c = #1 (c:HppaCells.cellset)
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     fun regSet c = #2 (c:HppaCells.cellset)
	     fun cellset((r,_),f) = (r,f))

  functor FloatRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=FregLiveness

 	   val defUse = P.defUse C.FP
	   val firstPseudoR = 32
	   val maxPseudoR = HppaCells.maxCell 
	   val numRegs = HppaCells.numCell HppaCells.FP
	   fun regSet c = #2 (c:HppaCells.cellset)
	end)
end

(*
 * $Log: hppaRegAlloc.sml,v $
 * Revision 1.3  1998/05/25 15:10:59  george
 *   Fixed RCS keywords
 *
 *)

(*
 * AMD64 specific backend.  This one uses the new RA8 scheme.
 *)
local
    val fast_floating_point =
	MLRiscControl.mkFlag ("amd64-fast-fp",
			      "whether to use the fast-fp backend (amd64)")
in
functor AMD64CG (structure CCallParams: sig val frameAlign : int
					    val returnSmallStructsInRegs : bool
					end
               val abi_variant: string option) =
  MachineGen
  ( structure I          = AMD64Instr
    structure C          = I.C
    structure F          = AMD64CFG
    structure R          = AMD64CpsRegs
    structure CG         = Control.CG

    structure MachSpec   = AMD64Spec
    val abi_variant      = abi_variant
    structure ClientPseudoOps = AMD64ClientPseudoOps
    structure PseudoOps  = AMD64PseudoOps
    structure Ext        = AMD64_SMLNJMLTreeExt(* amd64-specific *)
    structure CpsRegs    = AMD64CpsRegs
    structure InsnProps  = AMD64Props
    structure Asm        = AMD64AsmEmitter
    structure Shuffle    = AMD64Shuffle

    val fast_floating_point = fast_floating_point

    structure CCalls     = UnimplementedCCallsFn
			       (structure T = AMD64MLTree
				val impossible = ErrorMsg.impossible)

(*
    structure CCalls     = IA32SVID_CCalls (
        structure T = AMD64MLTree
        fun ix x = x
	val fast_floating_point = fast_floating_point
(* NOTE: the following need to be changed for MacOS X on Intel *)
	val frameAlign = CCallParams.frameAlign
	val returnSmallStructsInRegs = CCallParams.returnSmallStructsInRegs)
*)

    (* for the time being... *)
    structure OmitFramePtr = struct
      structure CFG=AMD64CFG
      structure I=AMD64Instr
      val vfp = CpsRegs.vfp
      (* no rewriting necessary, backend uses %fp instead of %sp *)
      fun omitframeptr _ = ()
    end
(*
    structure OmitFramePtr = 
      AMD64OmitFramePointer(structure I=AMD64Instr  
			  structure MemRegs=AMD64MemRegs
			  structure CFG=AMD64CFG
			  val memRegBase = SOME(AMD64CpsRegs.vfp))
*)

    val spill = CPSRegions.spill 
    val stack = CPSRegions.stack 

    fun error msg = MLRiscErrorMsg.error("AMD64CG",msg)

    fun base() = (* XXXX *)
      if !ClusterAnnotation.useVfp then AMD64CpsRegs.vfp else I.C.rsp 


    structure MLTreeComp=
       AMD64Gen(
           structure I=AMD64Instr
	   structure MLTreeUtils = MLTreeUtils
               (structure T = AMD64MLTree
                fun hashSext  _ _ = 0w0 
                fun hashRext  _ _ = 0w0
                fun hashFext  _ _ = 0w0 
                fun hashCCext _ _ = 0w0
             
                (* Equality extensions *)
                fun eqSext  _ _ = false
                fun eqRext  _ _ = false
                fun eqFext  _ _ = false
                fun eqCCext _ _ = false
             
                (* Pretty printing extensions *)
                fun showSext  _ _ = ""
                fun showRext  _ _ = ""
                fun showFext  _ _ = ""
                fun showCCext _ _ = ""
               )
           structure ExtensionComp = AMD64MLTreeExtComp
               (structure I = AMD64Instr
                structure T = AMD64MLTree
		structure CFG = AMD64CFG
		structure TS = AMD64MLTreeStream
		val fast_fp = fast_floating_point
               ) 
	   structure MLTreeStream = AMD64MLTreeStream
           fun cvti2f{src,ty,an} = let 
	     val tempMem = I.Displace{base=base(), disp=I.Immed 304, mem=stack}
           in
               {instrs  = [I.move{mvOp=I.MOVQ, src=src, dst=tempMem}],
                tempMem = tempMem,
                cleanup = []
               }
           end
          )

    structure Jumps = 
       AMD64Jumps(structure Instr=AMD64Instr
                  structure AsmEmitter=AMD64AsmEmitter
		  structure Eval=AMD64MLTreeEval 
                  structure Shuffle=AMD64Shuffle
                  structure MCEmitter=AMD64MCEmitter)
   
    structure BackPatch = 
       BackPatch(structure Jumps=Jumps
                 structure Emitter=AMD64MCEmitter
                 structure Props=InsnProps
		 structure CFG = AMD64CFG
                 structure Asm=AMD64AsmEmitter
                 structure CodeString=CodeString)

    structure RA = 
      AMD64RegAlloc
      (structure I         = AMD64Instr
       structure CB	   = CellsBasis
       structure Props = InsnProps
       structure Asm       = AMD64AsmEmitter
       structure CFG       = AMD64CFG
       structure SpillHeur = ChowHennessySpillHeur
       structure Spill     = RASpill
                             (structure Asm = AMD64AsmEmitter
                              structure InsnProps = InsnProps
                             )

       type spill_info = unit

       fun beforeRA _ = AMD64StackSpills.init()

       val toInt32 = Int32.fromInt
       fun cacheOffset r = I.Immed(toInt32(AMD64Runtime.vregStart + 
                                Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))
       fun cacheFPOffset f = I.Immed(toInt32(AMD64Runtime.vFpStart + 
                                Word.toIntX(Word.<<(Word.fromInt(f-40),0w3))))

       datatype ra_phase = SPILL_PROPAGATION | SPILL_COLORING
       datatype spill_operand_kind = SPILL_LOC | CONST_VAL

       structure Int =  
       struct
          val avail     = R.availR
          val dedicated = R.dedicatedR
          val phases    = [SPILL_PROPAGATION,SPILL_COLORING]

          (* We try to make unused memregs available for spilling 
           * This is necessary because of the stupid SML code generator
           * doesn't keep track of which are being used.
           *)
          fun spillInit(RAGraph.GRAPH{nodes, ...}) = 
          let val lookup = IntHashTable.lookup nodes
              fun find(r, free) =
                  if r >= 10 then (* note, %8 and %9 are reserved! *)
                     let val free = 
                             case lookup r of
                               RAGraph.NODE{uses=ref [], defs=ref [], ...} => 
                                  cacheOffset r::free
                             | _ => free
                     in  find(r-1, free) end
                  else 
                     free
              val free = find(31 (* AMD64Runtime.numVregs+8-1 *), [])
          in  (*AMD64StackSpills.setAvailableOffsets free*) ()
          end 
 
          val getRegLoc' = AMD64StackSpills.getRegLoc
 
          fun spillLoc{info, an, cell, id} = 
              {opnd=I.Displace{base=base(), disp=getRegLoc' id, mem=spill},
               kind=SPILL_LOC
              }
 
       end

       structure Float =
       struct
          val avail     = R.availF
          val dedicated = R.dedicatedF
          val phases    = [SPILL_PROPAGATION]

          fun spillInit(RAGraph.GRAPH{nodes, ...}) = 
              let val lookup = IntHashTable.lookup nodes
                 fun find(r, free) =
                     if r >= 32+8 then 
                        let val free = 
                                case lookup r of
                                  RAGraph.NODE{uses=ref [], defs=ref [],...} =>
                                     cacheFPOffset r::free
                                | _ => free
                        in  find(r-1, free) end
                     else 
                        free
                 val free = find(63, [])
              in AMD64StackSpills.setAvailableFPOffsets free
              end 

          fun spillLoc(S, an, loc) =
            I.Displace{base=base(), disp=AMD64StackSpills.getFregLoc loc, mem=spill}

      end
    ) (* AMD64RA *)
  ) (* AMD64CG *)
end

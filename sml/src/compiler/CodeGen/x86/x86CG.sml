(*
 * X86 specific backend.  This one uses the new RA8 scheme.
 *)
structure X86CG = 
  MachineGen
  ( structure I          = X86Instr
    structure C          = I.C
    structure F          = X86FlowGraph
    structure R          = X86CpsRegs
    structure CG         = Control.CG

    structure MachSpec   = X86Spec
    structure PseudoOps  = X86PseudoOps
    structure Ext        = X86_SMLNJMLTreeExt(* x86-specific *)
    structure CpsRegs    = X86CpsRegs
    structure InsnProps  = X86Props
    structure Asm        = X86AsmEmitter
    structure Shuffle    = X86Shuffle

    structure CCalls     = 
      IA32SVID_CCalls (structure T = X86MLTree  fun ix x = x)

    structure OmitFramePtr = 
      X86OmitFramePointer(structure I=X86Instr  structure F=X86FlowGraph 
			  structure PC=X86PrintCluster
			  structure MemRegs=X86MemRegs
			  val memRegBase = SOME(X86CpsRegs.vfp))

    val spill = CPSRegions.spill 
    val stack = CPSRegions.stack 

    fun error msg = MLRiscErrorMsg.error("X86CG",msg)

    val fast_floating_point = MLRiscControl.getFlag "x86-fast-fp"


    fun base() = (* XXXX *)
      if !ClusterAnnotation.useVfp then X86CpsRegs.vfp else I.C.esp 


    structure MLTreeComp=
       X86(structure X86Instr=X86Instr
           structure X86MLTree=X86MLTree
           structure ExtensionComp = X86MLTreeExtComp
               (structure I = X86Instr
                structure T = X86MLTree
               ) 
           structure MLTreeUtils = MLTreeUtils
               (structure T = X86MLTree
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
           fun cvti2f{src,ty,an} = let (* ty is always 32 for SML/NJ *)
	     val tempMem = I.Displace{base=base(), disp=I.Immed 304, mem=stack}
           in
               {instrs  = [I.MOVE{mvOp=I.MOVL, src=src, dst=tempMem}],
                tempMem = tempMem,
                cleanup = []
               }
           end
           datatype arch = Pentium | PentiumPro | PentiumII | PentiumIII
           val arch = ref Pentium (* Lowest common denominator *)
           val fast_floating_point = fast_floating_point
          )

    structure Jumps = 
       X86Jumps(structure Instr=X86Instr
                structure AsmEmitter=X86AsmEmitter
                structure Shuffle=X86Shuffle
                structure MCEmitter=X86MCEmitter)
   
    structure BackPatch = 
       BackPatch(structure Jumps=Jumps
                 structure Emitter=X86MCEmitter
                 structure Props=InsnProps
                 structure Flowgraph=X86FlowGraph
                 structure Asm=X86AsmEmitter
                 structure CodeString=CodeString)


    structure RA = 
      X86RA
      (structure I         = X86Instr
       structure InsnProps = InsnProps
       structure Asm       = X86AsmEmitter
       structure F         = X86FlowGraph
       structure SpillHeur = ChowHennessySpillHeur
       structure Spill     = RASpill
                             (structure Asm = X86AsmEmitter
                              structure InsnProps = InsnProps
                             )

       type spill_info = unit

       fun beforeRA _ = X86StackSpills.init()
       val fast_floating_point = fast_floating_point

       val toInt32 = Int32.fromInt
       fun cacheOffset r = I.Immed(toInt32(X86Runtime.vregStart + 
                                Word.toIntX(Word.<<(Word.fromInt(r-8),0w2))))
       fun cacheFPOffset f = I.Immed(toInt32(X86Runtime.vFpStart + 
                                Word.toIntX(Word.<<(Word.fromInt(f-40),0w3))))

       datatype raPhase = SPILL_PROPAGATION | SPILL_COLORING
       datatype spillOperandKind = SPILL_LOC | CONST_VAL

       structure Int =  
       struct
          val avail     = R.availR
          val dedicated = R.dedicatedR
          val memRegs   = C.Regs C.GP {from=8,to=31,step=1}
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
              val free = find(31 (* X86Runtime.numVregs+8-1 *), [])
          in  X86StackSpills.setAvailableOffsets free
          end 
 
          val getRegLoc' = X86StackSpills.getRegLoc
 
          fun spillLoc{info, an, cell, id} = 
              {opnd=I.Displace{base=base(), disp=getRegLoc' id, mem=spill},
               kind=SPILL_LOC
              }
 
       end

       structure Float =
       struct
          val avail     = R.availF
          val dedicated = R.dedicatedF
          val memRegs   = []
          val phases    = [SPILL_PROPAGATION]

          fun spillInit(RAGraph.GRAPH{nodes, ...}) = 
              if !fast_floating_point then
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
              in X86StackSpills.setAvailableFPOffsets free
              end 
              else ()

          fun spillLoc(S, an, loc) =
            I.Displace{base=base(), disp=X86StackSpills.getFregLoc loc, mem=spill}

          val fastMemRegs = C.Regs C.FP {from=8, to=31, step=1}
          val fastPhases  = [SPILL_PROPAGATION,SPILL_COLORING]
      end
    ) (* X86RA *)
  ) 


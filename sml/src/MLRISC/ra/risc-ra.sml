(* 
 * This functor factors out the machine independent part of the register
 * allocator.  It performs integer and floating register allocation.
 * This works well for RISC machines; but not applicable to x86.
 *)
functor RISC_RA
  (structure I         : INSTRUCTIONS
   structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
   structure Rewrite   : REWRITE_INSTRUCTIONS
   structure Asm       : INSTRUCTION_EMITTER

      (* Spilling heuristics determines which node should be spilled.
       * You can use Chaitin, ChowHenessey, or one of your own.
       *)
   structure SpillHeur : RA_SPILL_HEURISTICS

      (* The Spill module figures out the strategies for inserting
       * spill code.  You can use RASpill, or RASpillWithRenaming,
       * or write your own if you are feeling adventurous.
       *)
   structure Spill : RA_SPILL where I = I
          
   sharing InsnProps.I = Flowgraph.I = Asm.I = Rewrite.I = I
   sharing Asm.P = Flowgraph.P

   val architecture : string

   (* Is this a pure instruction *)
   val pure : I.instruction -> bool

   (* Called before RA begins *)
   val beginRA : unit -> unit

   structure Int :
   sig

      val avail     : I.C.cell list (* list of available registers *)
      val dedicated : I.C.cell list (* list of registers that are dedicated *)

      (* This functions is used to create copy instructions.
       * Given dst/src lists, return a new copy instruction with the same
       * temporary as the old one.
       *)
      val copy : (I.C.cell list * I.C.cell list) * I.instruction -> 
                     I.instruction

      (* This function is used to spill the temporary used in the copy
       * onto some stack offset.
       *)
      val spillCopyTmp : Annotations.annotations ref * I.instruction * 
                         RAGraph.spillLoc -> I.instruction

      (* This function is used to spill a register onto some stack offset 
       * The 
       *)
      val spillInstr : Annotations.annotations ref * I.C.cell * 
                       RAGraph.spillLoc -> I.instruction list
      (*
       * This function is used to reload a register from some stack offset
       *)
      val reloadInstr : Annotations.annotations ref * I.C.cell * 
                        RAGraph.spillLoc -> I.instruction list
   end

   structure Float :
   sig

      val avail     : I.C.cell list (* list of available registers *)
      val dedicated : I.C.cell list (* list of registers that are dedicated *)

      (* This functions is used to create copy instructions.
       * Given dst/src lists, return a new copy instruction with the same
       * temporary as the old one.
       *)
      val copy : (I.C.cell list * I.C.cell list) * I.instruction -> 
                     I.instruction

      (* This function is used to spill the temporary used in the copy
       * onto some stack offset.
       *)
      val spillCopyTmp : Annotations.annotations ref * I.instruction * 
                         RAGraph.spillLoc -> I.instruction

      (* This function is used to spill a register onto some stack offset 
       * The 
       *)
      val spillInstr : Annotations.annotations ref * I.C.cell * 
                       RAGraph.spillLoc -> I.instruction list
      (*
       * This function is used to reload a register from some stack offset,
       * and concatenate the reload code with the given instruction list.
       *)
      val reloadInstr : Annotations.annotations ref * I.C.cell * 
                        RAGraph.spillLoc -> I.instruction list
   end
  ) : CLUSTER_OPTIMIZATION =
struct

   structure F = Flowgraph
   structure I = F.I
   structure P = InsnProps
   structure C = I.C
   structure G = RAGraph

   type flowgraph = F.cluster

   val name = "RISC_RA"

   (* Counters for register allocation *)
   val intSpillsCnt = MLRiscControl.getCounter "ra-int-spills"
   val intReloadsCnt = MLRiscControl.getCounter "ra-int-reloads"
   val intRenamesCnt = MLRiscControl.getCounter "ra-int-renames"
   val floatSpillsCnt = MLRiscControl.getCounter "ra-float-spills"
   val floatReloadsCnt = MLRiscControl.getCounter "ra-float-reloads"
   val floatRenamesCnt = MLRiscControl.getCounter "ra-float-renames"

   fun error msg = MLRiscErrorMsg.error("RISC RA "^architecture,msg)

   (*
    * Make arithmetic non-overflow trapping.
    * This makes sure that if we happen to run the compiler for a long
    * period of time overflowing counters will not crash the compiler. 
    *)
   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))
   fun x - y = Word.toIntX(Word.-(Word.fromInt x, Word.fromInt y))

   (* GetReg specialized to integer and floating point registers *)
   local
      val {low,high} = C.cellRange C.GP
   in
       structure GR = GetReg(val first=low val nRegs=high-low+1 
                             val available=map C.registerId Int.avail)
       val dedicatedR = Array.array(high+1,false)
       val _ = app (fn r => Array.update(dedicatedR,C.registerId r,true)) 
                            Int.dedicated

   end
   local 
      val {low,high} = C.cellRange C.FP
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=map C.registerId Float.avail)
      val dedicatedF = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedF,C.registerId r,true)) 
                      Float.dedicated
   end

   (* Spill integer register *)
   fun spillR{annotations,kill=true,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillR{annotations=annotations,kill=false,
                     spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillR{annotations,kill,reg,spillLoc,instr} = 
       let val _   = intSpillsCnt := !intSpillsCnt + 1
           val newR = C.newReg()
    	   val instr' = Rewrite.rewriteDef(instr, reg, newR)
       in  {code=instr'::Int.spillInstr(annotations,newR,spillLoc), 
            proh=[newR], newReg=SOME newR}
       end

   fun spillReg{annotations,src,reg,spillLoc} =
       (intSpillsCnt := !intSpillsCnt + 1;
        Int.spillInstr(annotations,src,spillLoc) 
       )

   fun spillTmp{annotations,copy,spillLoc} =
       (intSpillsCnt := !intSpillsCnt + 1;
        Int.spillCopyTmp(annotations,copy,spillLoc)
       )

   (* Spill floating point register *)
   fun spillF{annotations,kill=true,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillF{annotations=annotations,kill=false,
                     spillLoc=spillLoc, reg=reg,instr=instr}
     | spillF{annotations,kill,reg,spillLoc,instr} = 
       let val _   = floatSpillsCnt := !floatSpillsCnt + 1
           val newR = C.newFreg()
    	   val instr' = Rewrite.frewriteDef(instr, reg, newR)
       in {code=instr'::Float.spillInstr(annotations,newR,spillLoc), 
           proh=[newR], newReg=SOME newR}
       end

   fun spillFreg{annotations,reg,src,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        Float.spillInstr(annotations,src,spillLoc)
       )

   fun spillFtmp{annotations,copy,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        Float.spillCopyTmp(annotations,copy,spillLoc) 
       )

   (* Rename integer register *)
   fun renameR{fromSrc,toSrc,instr} = 
       let val _   = intRenamesCnt := !intRenamesCnt + 1
           val instr' = Rewrite.rewriteUse(instr, fromSrc, toSrc)
       in {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload integer register *)
   fun reloadR{annotations,reg,spillLoc,instr} = 
       let val _   = intReloadsCnt := !intReloadsCnt + 1
           val newR = C.newReg()
           val instr' = Rewrite.rewriteUse(instr, reg, newR)
       in {code=Int.reloadInstr(annotations,newR,spillLoc) @ [instr'], 
           proh=[newR], newReg=SOME newR}
       end

   fun reloadReg{annotations,reg,dst,spillLoc} = 
       (intReloadsCnt := !intReloadsCnt + 1;
        Int.reloadInstr(annotations,dst,spillLoc) 
       )
                   
   (* Rename floating point register *)
   fun renameF{fromSrc,toSrc,instr} =
       let val _ = floatRenamesCnt := !floatRenamesCnt + 1
           val instr' = Rewrite.frewriteUse(instr, fromSrc, toSrc)
       in  {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload floating point register *)
   fun reloadF{annotations,reg,spillLoc,instr} =
       let val _ = floatReloadsCnt := !floatReloadsCnt + 1
           val newR = C.newFreg()
           val instr' = Rewrite.frewriteUse(instr, reg, newR)
       in  {code=Float.reloadInstr(annotations,newR,spillLoc) @ [instr'], 
            proh=[newR], newReg=SOME newR}
       end

   fun reloadFreg{annotations,reg,dst,spillLoc} =
       (floatReloadsCnt := !floatReloadsCnt + 1;
        Float.reloadInstr(annotations,dst,spillLoc) 
       )

   (* The generic register allocator *)
   structure Ra =
      RegisterAllocator
        (SpillHeur) 
        (* (ChowHennessySpillHeur) *)
        (ClusterRA 
          (structure Flowgraph = F
           structure Asm = Asm
           structure InsnProps = InsnProps
           structure Spill = Spill
          )
        )

   val KR = length Int.avail
   val KF = length Float.avail

   val params =
       [  { cellkind     = I.C.GP,
            getreg       = GR.getreg,
            spill        = spillR,
            spillSrc     = spillReg,
            spillCopyTmp = spillTmp,
            reload       = reloadR,
            reloadDst    = reloadReg,
            renameSrc    = renameR,
            K            = KR,
            dedicated    = dedicatedR,
            copyInstr    = fn i => [Int.copy i],
            spillProh    = [],
            memRegs      = [],
            mode         = Ra.NO_OPTIMIZATION
          } : Ra.raClient,
          { cellkind     = I.C.FP,
            getreg       = FR.getreg,
            spill        = spillF,
            spillSrc     = spillFreg,
            spillCopyTmp = spillFtmp,
            reload       = reloadF,
            reloadDst    = reloadFreg,
            renameSrc    = renameF,
            K            = KF,
            dedicated    = dedicatedF,
            copyInstr    = fn i => [Float.copy i],
            spillProh    = [],
            memRegs      = [],
            mode         = Ra.NO_OPTIMIZATION
          } : Ra.raClient
       ] : Ra.raClient list
  
   fun run cluster =
      (beginRA();
       GR.reset();
       FR.reset();
       Ra.ra params cluster
      )

end

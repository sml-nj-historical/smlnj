(* 
 * This functor factors out the machine independent part of the register
 * allocator.  It performs integer and floating register allocation.
 * This works well for RISC machines; but not applicable to x86.
 *)
functor RISC_RA
  (structure I         : INSTRUCTIONS
   structure Asm       : INSTRUCTION_EMITTER
   			where I = I 
   structure Flowgraph : CONTROL_FLOW_GRAPH 
   			where I = I
		          and P = Asm.S.P
   structure InsnProps : INSN_PROPERTIES
   			where I = I
   structure Rewrite   : REWRITE_INSTRUCTIONS
   			where I = I

      (* Spilling heuristics determines which node should be spilled.
       * You can use Chaitin, ChowHenessey, or one of your own.
       *)
   structure SpillHeur : RA_SPILL_HEURISTICS

      (* The Spill module figures out the strategies for inserting
       * spill code.  You can use RASpill, or RASpillWithRenaming,
       * or write your own if you are feeling adventurous.
       *)
   structure Spill : RA_SPILL where I = I
          
   val architecture : string

   (* Is this a pure instruction *)
   val pure : I.instruction -> bool

   (* Called before RA begins *)
   val beginRA : unit -> unit

   structure Int :
   sig

      val avail     : CellsBasis.cell list (* list of available registers *)
      val dedicated : CellsBasis.cell list (* list of registers that are dedicated *)

      (* This functions is used to create copy instructions.
       * Given dst/src lists, return a new copy instruction with the same
       * temporary as the old one.
       *)
      val copy : (CellsBasis.cell list * CellsBasis.cell list) * I.instruction -> 
                     I.instruction

      (* This function is used to spill the temporary used in the copy
       * onto some stack offset.
       *)
      val spillCopyTmp : Annotations.annotations ref * I.instruction * 
                         RAGraph.spillLoc -> I.instruction

      (* This function is used to spill a register onto some stack offset 
       *)
      val spillInstr : {an:Annotations.annotations ref, src:CellsBasis.cell,
			spilledCell:CellsBasis.cell, spillLoc:RAGraph.spillLoc} 
	               -> I.instruction list

      (*
       * This function is used to reload a register from some stack offset
       *)
      val reloadInstr : {an:Annotations.annotations ref, dst:CellsBasis.cell,
			 spilledCell:CellsBasis.cell, spillLoc:RAGraph.spillLoc}
	                -> I.instruction list

      (* Mode for RA optimizations *)
      val mode : RAGraph.mode
   end

   structure Float :
   sig

      val avail     : CellsBasis.cell list (* list of available registers *)
      val dedicated : CellsBasis.cell list (* list of registers that are dedicated *)

      (* This functions is used to create copy instructions.
       * Given dst/src lists, return a new copy instruction with the same
       * temporary as the old one.
       *)
      val copy : (CellsBasis.cell list * CellsBasis.cell list) * I.instruction -> 
                     I.instruction

      (* This function is used to spill the temporary used in the copy
       * onto some stack offset.
       *)
      val spillCopyTmp : Annotations.annotations ref * I.instruction * 
                         RAGraph.spillLoc -> I.instruction

      (* This function is used to spill a register onto some stack offset 
       * The 
       *)
      val spillInstr : Annotations.annotations ref * CellsBasis.cell * 
                       RAGraph.spillLoc -> I.instruction list
      (*
       * This function is used to reload a register from some stack offset,
       * and concatenate the reload code with the given instruction list.
       *)
      val reloadInstr : Annotations.annotations ref * CellsBasis.cell * 
                        RAGraph.spillLoc -> I.instruction list

      (* Mode for RA optimizations *)
      val mode : RAGraph.mode
   end
  ) : CFG_OPTIMIZATION =
struct

   structure CFG = Flowgraph
   structure I   = CFG.I
   structure P   = InsnProps
   structure C   = I.C
   structure G   = RAGraph

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
   fun isDedicated (len, arr, others) r = 
     (r < len andalso Array.sub(arr, r)) orelse List.exists (fn d => r = d) others

   fun mark(arr, _, [], others) = others
     | mark(arr, len, r::rs, others) = let
	 val r = CellsBasis.registerId r
       in
	 if r >= len then mark(arr, len, rs, r::others)
	 else (Array.update(arr, r, true); mark(arr, len, rs, others))
       end



   local
       val {low,high} = C.cellRange CellsBasis.GP
       val arr = Array.array(high+1,false)
       val others = mark(arr, high+1, Int.dedicated, [])
   in
       structure GR = GetReg(val first=low val nRegs=high-low+1 
                             val available=map CellsBasis.registerId Int.avail)
       val dedicatedR : int -> bool = isDedicated (high+1, arr, others)
   end
   local 
      val {low,high} = C.cellRange CellsBasis.FP
      val arr = Array.array(high+1,false)
      val others = mark(arr, high+1, Float.dedicated, [])
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=map CellsBasis.registerId Float.avail)
      val dedicatedF : int -> bool = isDedicated(high+1, arr, others)
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
       in  {code=instr'::Int.spillInstr{an=annotations,src=newR,
					spilledCell=reg,spillLoc=spillLoc}, 
            proh=[newR], newReg=SOME newR}
       end

   fun spillReg{annotations,src,reg,spillLoc} =
       (intSpillsCnt := !intSpillsCnt + 1;
        Int.spillInstr{an=annotations,src=src,spilledCell=reg,
		       spillLoc=spillLoc}
       )

   fun spillTmp{annotations,reg,copy,spillLoc} =
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

   fun spillFtmp{annotations,reg,copy,spillLoc} = 
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
       in {code=Int.reloadInstr{an=annotations,dst=newR,spilledCell=reg,
				spillLoc=spillLoc} @ [instr'], 
           proh=[newR], newReg=SOME newR}
       end

   fun reloadReg{annotations,reg,dst,spillLoc} = 
       (intReloadsCnt := !intReloadsCnt + 1;
        Int.reloadInstr{an=annotations,dst=dst,spilledCell=reg,
			spillLoc=spillLoc}
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
          (structure Flowgraph = CFG
           structure Asm = Asm
           structure InsnProps = InsnProps
           structure Spill = Spill
          )
        )

   val KR = length Int.avail
   val KF = length Float.avail

   val params =
       [  { cellkind     = CellsBasis.GP,
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
            mode         = Int.mode
          } : Ra.raClient,
          { cellkind     = CellsBasis.FP,
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
            mode         = Float.mode
          } : Ra.raClient
       ] : Ra.raClient list
  
   fun run cluster =
      (beginRA();
       GR.reset();
       FR.reset();
       Ra.ra params cluster
      )

end


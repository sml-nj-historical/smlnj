(* 
 * This functor factors out the machine independent part of the register
 * allocator.   This works well for RISC machines; not applicable to x86.
 *)
functor RegAlloc
  (structure I         : INSTRUCTIONS
   structure MachSpec  : MACH_SPEC
   structure Flowgraph : FLOWGRAPH
   structure CpsRegs   : CPSREGS
   structure InsnProps : INSN_PROPERTIES
   structure Rewrite   : REWRITE_INSTRUCTIONS
   structure Asm       : INSTRUCTION_EMITTER
      sharing CpsRegs.T.Constant=Flowgraph.I.Constant
      sharing InsnProps.I = Flowgraph.I = Asm.I = Rewrite.I = I
      sharing Asm.P = CpsRegs.T.PseudoOp = Flowgraph.P

   (* Is this a pure instruction *)
   val pure : I.instruction -> bool

   (* These functions are used to create copy instructions for
    * integer and floating point registers.  Given dst/src lists,
    * and a copy instruction, return a new copy instruction with the same
    * temporary as the old one.
    *)
   val copyR : (int list * int list) * I.instruction -> I.instruction 
   val copyF : (int list * int list) * I.instruction -> I.instruction 

   (* These functions are used to spill the temporary used in the copy
    * onto some stack offset.
    *)
   val spillCopyTmp : I.instruction * int -> I.instruction
   val spillFcopyTmp : I.instruction * int -> I.instruction

   (*
    * These functions are used to spill a register onto some stack offset
    *)
   val spillInstrR : I.C.cell * int -> I.instruction list
   val spillInstrF : I.C.cell * int -> I.instruction list

   (*
    * These functions are used to reload a register from some stack offset,
    * and concatenate the reload code with the given instruction list.
    *)
   val reloadInstrR : I.C.cell * int * I.instruction list -> I.instruction list
   val reloadInstrF : I.C.cell * int * I.instruction list -> I.instruction list
  ) : REGALLOC =
struct

   structure F     = Flowgraph
   structure I     = F.I
   structure P     = InsnProps
   structure Cells = I.C
  
   (* Counters for register allocation *)
   val intSpillsCnt = MLRiscControl.getCounter "ra-int-spills"
   val intReloadsCnt = MLRiscControl.getCounter "ra-int-reloads"
   val intRenamesCnt = MLRiscControl.getCounter "ra-int-renames"
   val floatSpillsCnt = MLRiscControl.getCounter "ra-float-spills"
   val floatReloadsCnt = MLRiscControl.getCounter "ra-float-reloads"
   val floatRenamesCnt = MLRiscControl.getCounter "ra-float-renames"

   fun error msg = MLRiscErrorMsg.error(MachSpec.architecture,msg)

   val itow = Word.fromInt

   (*
    * Make arithmetic non-overflow trapping.
    * This makes sure that if we happen to run the compiler for a long
    * period of time overflowing counters will not crash the compiler. 
    *)
   fun x + y = Word.toIntX(Word.+(Word.fromInt x, Word.fromInt y))
   fun x - y = Word.toIntX(Word.-(Word.fromInt x, Word.fromInt y))

   (* GetReg specialized to integer and floating point registers *)
   local
      val {low,high} = Cells.cellRange Cells.GP
   in
      structure GR = GetReg(val first=low val nRegs=high-low+1 
                            val available=CpsRegs.availR)
       val dedicatedR = Array.array(high+1,false)
       val _ = app (fn r => Array.update(dedicatedR,r,true)) CpsRegs.dedicatedR

   end
   local 
      val {low,high} = Cells.cellRange Cells.FP
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=CpsRegs.availF)
      val dedicatedF = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedF,r,true)) CpsRegs.dedicatedF
   end

   exception RegSpills and FregSpills
   val initialSpillOffset = MachSpec.initialSpillOffset
   val spillOffset = ref initialSpillOffset
   val spillAreaSz = MachSpec.spillAreaSz
   val regspills : int Intmap.intmap = Intmap.new(0,RegSpills)
   val fregspills : int Intmap.intmap = Intmap.new(0,FregSpills)
   val lookupReg  = Intmap.map regspills
   val enterReg   = Intmap.add regspills
   val lookupFreg = Intmap.map fregspills
   val enterFreg  = Intmap.add fregspills

   fun spillInit() = 
      ((* Reset the regspills/fregspills map by need. *)
       if !spillOffset = initialSpillOffset then ()
       else (Intmap.clear regspills; 
             Intmap.clear fregspills 
            )
       ;
       spillOffset := initialSpillOffset
      )

   fun newOffset offset = 
       if offset >= spillAreaSz then error "spill area too small"
       else spillOffset := offset

   (* Get spill location for integer registers *)
   fun getRegLoc reg = lookupReg reg handle _ =>
       let val offset = !spillOffset
       in  newOffset(offset+4); 
           enterReg (reg,offset);
           offset
       end

   (* Get spill location for floating point registers *)
   fun getFregLoc freg = lookupFreg freg handle _ =>
       let val offset = !spillOffset
  	   val aligned = Word.toIntX (Word.andb(itow (offset+7), itow ~8))
       in
           newOffset(aligned+8);
	   enterFreg (freg, aligned);
	   aligned
       end

   (* Spill integer register *)
   fun spillR{annotations,kill=true,regmap,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillR{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillR{annotations,kill,regmap,reg,spillLoc,instr} = 
       let val _   = intSpillsCnt := !intSpillsCnt + 1
           val loc = getRegLoc spillLoc
           val newR = Cells.newReg()
    	   val instr' = Rewrite.rewriteDef(regmap, instr, reg, newR)
       in  {code=instr'::spillInstrR(newR,loc), proh=[newR], newReg=SOME newR}
       end

   fun spillReg{annotations,src,reg,spillLoc} =
       (intSpillsCnt := !intSpillsCnt + 1;
        spillInstrR(src,getRegLoc spillLoc) 
       )

   fun spillTmp{annotations,copy,spillLoc} =
       (intSpillsCnt := !intSpillsCnt + 1;
        spillCopyTmp(copy,getRegLoc spillLoc)
       )

   (* Spill floating point register *)
   fun spillF{annotations,kill=true,regmap,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillF{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillF{annotations,kill,regmap,reg,spillLoc,instr} = 
       let val _   = floatSpillsCnt := !floatSpillsCnt + 1
           val loc = getFregLoc spillLoc
           val newR = Cells.newFreg()
    	   val instr' = Rewrite.frewriteDef(regmap, instr, reg, newR)
       in {code=instr'::spillInstrF(newR,loc), proh=[newR], newReg=SOME newR}
       end

   fun spillFreg{annotations,reg,src,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        spillInstrF(src,getFregLoc spillLoc)
       )

   fun spillFtmp{annotations,copy,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        spillFcopyTmp(copy,getFregLoc spillLoc) 
       )

   (* Rename integer register *)
   fun renameR{regmap,fromSrc,toSrc,instr} = 
       let val _   = intRenamesCnt := !intRenamesCnt + 1
           val instr' = Rewrite.rewriteUse(regmap, instr, fromSrc, toSrc)
       in {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload integer register *)
   fun reloadR{annotations,regmap,reg,spillLoc,instr} = 
       let val _   = intReloadsCnt := !intReloadsCnt + 1
           val loc = getRegLoc spillLoc
           val newR = Cells.newReg()
           val instr' = Rewrite.rewriteUse(regmap, instr, reg, newR)
       in {code=reloadInstrR(newR,loc,[instr']), proh=[newR], newReg=SOME newR}
       end

   fun reloadReg{annotations,reg,dst,spillLoc} = 
       (intReloadsCnt := !intReloadsCnt + 1;
        reloadInstrR(dst,getRegLoc spillLoc,[]) 
       )
                   
   (* Rename floating point register *)
   fun renameF{regmap,fromSrc,toSrc,instr} =
       let val _ = floatRenamesCnt := !floatRenamesCnt + 1
           val instr' = Rewrite.frewriteUse(regmap, instr, fromSrc, toSrc)
       in  {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload floating point register *)
   fun reloadF{annotations,regmap,reg,spillLoc,instr} =
       let val _ = floatReloadsCnt := !floatReloadsCnt + 1
           val loc = getFregLoc(spillLoc)
           val newR = Cells.newFreg()
           val instr' = Rewrite.frewriteUse(regmap, instr, reg, newR)
       in  {code=reloadInstrF(newR,loc,[instr']), proh=[newR], newReg=SOME newR}
       end

   fun reloadFreg{annotations,reg,dst,spillLoc} =
       (floatReloadsCnt := !floatReloadsCnt + 1;
        reloadInstrF(dst,getFregLoc spillLoc,[]) 
       )

   (* The generic register allocator *)
   structure Ra =
      RegisterAllocator
        (ChaitinSpillHeur) 
        (* (ChowHennessySpillHeur) *)
        (ClusterRA 
          (structure Flowgraph = F
           structure Asm = Asm
           structure InsnProps = InsnProps
           structure Spill = RASpill
             (structure Asm = Asm
              structure InsnProps = InsnProps
             )
          )
        )

   val KR = length CpsRegs.availR
   val KF = length CpsRegs.availF

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
            copyInstr    = fn i => [copyR i],
            spillProh    = [],
            memRegs      = [],
            mode         = Ra.NO_OPTIMIZATION
          },
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
            copyInstr    = fn i => [copyF i],
            spillProh    = [],
            memRegs      = [],
            mode         = Ra.NO_OPTIMIZATION
          }
       ] : Ra.raClient list
  
   fun ra cluster =
      (spillInit();
       GR.reset();
       FR.reset();
       Ra.ra params cluster
      )

end

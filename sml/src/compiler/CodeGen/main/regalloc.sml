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
   val floatSpillsCnt = MLRiscControl.getCounter "ra-float-spills"
   val floatReloadsCnt = MLRiscControl.getCounter "ra-float-reloads"

   fun error msg = MLRiscErrorMsg.error(MachSpec.architecture,msg)

   val itow = Word.fromInt

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
   fun spillR{annotations,kill=true,regmap,reg,spillLoc,graph,instr} = 
         if pure instr then {code=[], instr=NONE, proh=[]}
         else spillR{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,graph=graph,instr=instr}
     | spillR{annotations,kill,regmap,reg,spillLoc,graph,instr} = 
       let val _   = intSpillsCnt := !intSpillsCnt + 1
           val loc = getRegLoc(spillLoc)
           fun spillIt() = 
           let val newR = Cells.newReg()
    	       val instr' = Rewrite.rewriteDef(regmap, instr, reg, newR)
           in {code=spillInstrR(newR,loc), instr=SOME instr', proh=[newR]}
           end
       in  case P.instrKind instr of
             P.IK_COPY => 
               (case P.moveDstSrc instr of 
                  ([rd], [rs]) => 
                     if regmap rd = reg then 
                       {code=spillInstrR(rs,loc), instr=NONE,proh=[]}
                     else   
                     (case P.moveTmpR instr of
                       NONE => error "spillR: MOVE"
                     | SOME r => let val newCopy = spillCopyTmp(instr,loc)
                                 in {code=[], instr=SOME newCopy, proh=[]} end
                     )
                | _ => spillIt()
                )
           | _ => spillIt()
       end

   (* Spill floating point register *)
   fun spillF{annotations,kill=true,regmap,reg,spillLoc,graph,instr} = 
         if pure instr then {code=[], instr=NONE, proh=[]}
         else spillF{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,graph=graph,instr=instr}
     | spillF{annotations,kill,regmap,reg,spillLoc,graph,instr} = 
       let val _   = floatSpillsCnt := !floatSpillsCnt + 1
           val loc = getFregLoc(spillLoc)
           fun spillIt() =
           let val newR = Cells.newFreg()
    	       val instr' = Rewrite.frewriteDef(regmap, instr, reg, newR)
           in {code=spillInstrF(newR,loc), instr=SOME instr', proh=[newR]}
           end
    
       in  case P.instrKind instr of
             P.IK_COPY => 
               (case P.moveDstSrc instr of 
                  ([fd], [fs]) => 
                     if regmap fd = reg then 
                      {code=spillInstrF(fs,loc), instr=NONE,proh=[]}
                     else   
                     (case P.moveTmpR instr of
                       NONE => error "spillF: MOVE"
                     | SOME r => let val newCopy = spillFcopyTmp(instr,loc)
                                 in {code=[], instr=SOME newCopy, proh=[]} end
                     )
                | _ => spillIt()
                )
           | _ => spillIt()
       end

   (* Reload integer register *)
   fun reloadR{annotations,regmap,reg,spillLoc,graph,instr} = 
   let val _   = intReloadsCnt := !intReloadsCnt + 1
       val loc = getRegLoc(spillLoc)
       fun reloadIt() =
       let val newR = Cells.newReg()
	   val instr' = Rewrite.rewriteUse(regmap, instr, reg, newR)
       in {code=reloadInstrR(newR,loc,[instr']), proh=[newR]}
       end
   in  case P.instrKind instr of
         P.IK_COPY => 
           (case P.moveDstSrc instr of
             ([rd],[rs]) => {code=reloadInstrR(rd,loc,[]), proh=[]}
           | _ => reloadIt()  
           )
       | _ => reloadIt()
   end
                   
   (* Reload floating point register *)
   fun reloadF{annotations,regmap,reg,spillLoc,graph,instr} =
   let val _   = floatReloadsCnt := !floatReloadsCnt + 1
       val loc = getFregLoc(spillLoc)
       fun reloadIt() =
       let val newR = Cells.newFreg()
	   val instr' = Rewrite.frewriteUse(regmap, instr, reg, newR)
       in {code=reloadInstrF(newR,loc,[instr']), proh=[newR]}
       end
   in  case P.instrKind instr of
         P.IK_COPY => 
           (case P.moveDstSrc instr of
             ([fd],[fs]) => {code=reloadInstrF(fd,loc,[]), proh=[]}
           | _ => reloadIt()  
           )
       | _ => reloadIt()
   end

   (* The generic register allocator *)
   structure Ra =
      RegisterAllocator
        (ChaitinSpillHeur)
        (ClusterRA 
          (structure Flowgraph = F
           structure Asm = Asm
           structure InsnProps = InsnProps
          )
        )

   fun cp _ = error "copy propagation"

   val KR = length CpsRegs.availR
   val KF = length CpsRegs.availF

   val params =
       [  { cellkind  = I.C.GP,
            getreg    = GR.getreg,
            spill     = spillR,
            reload    = reloadR,
            K         = KR,
            dedicated = dedicatedR,
            copyInstr = copyR,
            spillProh = [],
            optimizations = []
          },
          { cellkind  = I.C.FP,
            getreg    = FR.getreg,
            spill     = spillF,
            reload    = reloadF,
            K         = KF,
            dedicated = dedicatedF,
            copyInstr = copyF,
            spillProh = [],
            optimizations = []
          }
       ]
  
   fun ra cluster =
      (spillInit();
       GR.reset();
       FR.reset();
       Ra.ra Ra.REGISTER_ALLOCATION params cluster
      )

end

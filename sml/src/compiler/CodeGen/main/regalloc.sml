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

   (* The following is the signature of the higher order functor
    * provided by MLRISC.  So our functor RegAlloc is another order higher!
    *)
   functor Ra(  
      structure I : INSTRUCTIONS = Flowgraph.I
      structure P : INSN_PROPERTIES where I = I 
      structure F : FLOWGRAPH = Flowgraph
      structure Asm : INSTRUCTION_EMITTER = Asm
   ) : 
   sig
      functor IntRa (structure RaUser : RA_USER_PARAMS
                     where I = I and B = Flowgraph.B) : RA where F = Flowgraph
      functor FloatRa (structure RaUser : RA_USER_PARAMS
                       where I = I and B = Flowgraph.B) : RA where F = Flowgraph
   end

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
   structure B     = F.B
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
   end
   local 
      val {low,high} = Cells.cellRange Cells.FP
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=CpsRegs.availF)
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
   fun spillR{id,regmap,reg,instr} = 
   let val _   = intSpillsCnt := !intSpillsCnt + 1
       val loc = getRegLoc(reg)
       fun spillIt() = 
       let val newR = Cells.newReg()
	   val instr' = Rewrite.rewriteDef(regmap, instr, reg, newR)
       in {code=spillInstrR(newR,loc), instr=SOME instr', proh=[newR]}
       end
   in  case P.instrKind instr of
         P.IK_COPY => 
           (case P.moveDstSrc instr of 
              ([rd], [rs]) => 
                 (* Warning: should go thru the regmap for correctness! *)
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
   fun spillF{id,regmap,reg,instr} = 
   let val _   = floatSpillsCnt := !floatSpillsCnt + 1
       val loc = getFregLoc(reg)
       fun spillIt() =
       let val newR = Cells.newFreg()
	   val instr' = Rewrite.frewriteDef(regmap, instr, reg, newR)
       in {code=spillInstrF(newR,loc), instr=SOME instr', proh=[newR]}
       end

   in  case P.instrKind instr of
         P.IK_COPY => 
           (case P.moveDstSrc instr of 
              ([fd], [fs]) => 
                 (* Warning: should go thru the regmap for correctness! *)
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
   fun reloadR{id,regmap,reg,instr} = 
   let val _   = intReloadsCnt := !intReloadsCnt + 1
       val loc = getRegLoc(reg)
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
   fun reloadF{id,regmap,reg,instr} =
   let val _   = floatReloadsCnt := !floatReloadsCnt + 1
       val loc = getFregLoc(reg)
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

   (* Instantiate the register allocator *)
   structure RegAlloc =
       Ra(structure I   = I
          structure P   = InsnProps
          structure F   = F
          structure Asm = Asm
         )

   (* Integer register allocator *)
   structure IntRa =
      RegAlloc.IntRa
        (structure RaUser = 
         struct
            structure I = I
            structure B = B
            val getreg = GR.getreg
            val spill  = spillR
            val reload = reloadR
            val nFreeRegs = length CpsRegs.availR
            val dedicated = CpsRegs.dedicatedR
            val copyInstr = copyR
         end
        )

   (* Floating point register allocator *)
   structure FloatRa =
      RegAlloc.FloatRa
        (structure RaUser = 
         struct
            structure I = I
            structure B = B
            val getreg = FR.getreg
            val spill  = spillF
            val reload = reloadF
            val nFreeRegs = length CpsRegs.availF
            val dedicated = CpsRegs.dedicatedF
            val copyInstr = copyF
         end
        )

   val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION []
   val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION []
   val icp       = IntRa.ra IntRa.COPY_PROPAGATION []
   val fcp       = FloatRa.ra FloatRa.COPY_PROPAGATION []
   val cp        = fcp o icp

   fun ra cluster =
   let fun intRa cluster   = (GR.reset(); iRegAlloc cluster)
       fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
   in  spillInit();
       floatRa(intRa cluster)
   end

end

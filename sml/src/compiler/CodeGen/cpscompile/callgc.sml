(* callGc.sml --- cluster of gc invocation code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)
functor CallGc
  (structure MLTreeComp : MLTREECOMP
   structure Cells : CELLS
   structure C : CPSREGS where T.Region=CPSRegions
   structure MS: MACH_SPEC
   structure ConstType : CONST_TYPE
     sharing MLTreeComp.T = C.T
     sharing type C.T.Constant.const = ConstType.const) : CALLGC = 
struct
  structure T : MLTREE = MLTreeComp.T
  structure Const = ConstType
  structure D = MS.ObjDesc
  structure LE = LabelExp
  structure R = CPSRegions

  type t = {maxAlloc: int,
	    regfmls:  T.mlrisc list,
	    regtys : CPS.cty list,
	    return: T.stm} 

  (* GcInfo contains information to generate code to invoke gc *)
  datatype gcInfo = 
    GCINFO of {lab: Label.label ref,  
	       maskRegs: T.rexp list, 
	       i32Regs: T.rexp list,
	       fRegs: T.fexp list,
	       ret: T.stm}

  fun error msg = ErrorMsg.impossible ("CallGc." ^ msg)

  val emit = MLTreeComp.mlriscComp
  val comp = MLTreeComp.mltreeComp

  val dedicated = 
    map (T.GPR o T.REG) C.dedicatedR @ map (T.FPR o T.FREG) C.dedicatedF

  fun equal
       (GCINFO{maskRegs=mr1, i32Regs=i1, fRegs=f1, ret=T.JMP(ret1,_), ...},
        GCINFO{maskRegs=mr2, i32Regs=i2, fRegs=f2, ret=T.JMP(ret2,_), ...}) = 
       let
        fun eqRoot(T.REG x, T.REG y) = x = y
	  | eqRoot(T.LOAD32(ea1, _), T.LOAD32(ea2, _)) = 
	     (case (ea1, ea2) 
	      of (T.REG r1, T.REG r2) => r1 = r2
	       | (T.ADD(T.REG r1, T.LI i),  T.ADD(T.REG r2, T.LI j)) => 
		    r1=r2 andalso i=j
	      (*esac*))

	fun eqR([], []) = true
	  | eqR(x::xs, y::ys) = eqRoot(x, y) andalso eqR(xs, ys)
	  | eqR _ = false

	(* This may need to be more sophisticated whenever floating 
	 * registers get represented by memory locations.
	 *)
	fun eqF([], []) = true
	  | eqF(T.FREG(x)::xs, T.FREG(y)::ys) = x=y andalso eqF(xs, ys)
	  | eqF _ = false
      in
	eqR(mr1, mr2) andalso eqR(i1, i2) andalso 
	eqF(f1, f2) andalso eqRoot(ret1,ret2)
      end
    | equal _ = false

  val clusterGcBlocks = ref ([] : gcInfo list)
  val knownGcBlocks = ref([]: gcInfo list)
  val moduleGcBlocks = ref ([] : gcInfo list)

  (* (+ 8) for descriptor and possible alignment *)
  val falloc = MS.numFloatRegs * 8 + 8 

  (* generate the CHECKLIMIT *)
  fun checkLimit (maxAlloc, regtys) = let
    val lab = Label.newLabel ""
    val max_alloc = maxAlloc + falloc
    fun assignCC(T.CC cc, v) = T.CCMV(cc, v)
      | assignCC(T.LOADCC(ea,region), v) = T.STORECC(ea, v, region)
      | assignCC _ = error "checkLimit.assign"
  in
    if max_alloc < 4096 then () 
    else let 
        val allocptr' = T.ADD(C.allocptr, T.LI(max_alloc-4096))
      in emit(assignCC(C.exhausted, T.CMP(T.GTU, allocptr', C.limitptr, T.LR)))
      end;
    emit(T.BCC(T.GTU, C.exhausted, lab));
    lab
  end

  (* floating point registers and INT32t paramaters do not 
   * appear in the mask.
   *)
  fun checkStackPtr(T.REG r) = 
    if r <> Cells.stackptrR then error "checkStackPtr" else ()

  fun maskList([],[], mask, i32mask, fregs) = (mask, i32mask, fregs)
    | maskList(T.GPR r::rl, CPS.INT32t::tl, mask, i32, fregs) = 
       maskList(rl, tl, mask, r::i32, fregs)
    | maskList(T.FPR r::rl, CPS.FLTt::tl, mask, i32, fregs) = 
       maskList(rl, tl, mask, i32, r::fregs)
    | maskList(T.GPR r::rl, _::tl, mask, i32, fregs) = 
       maskList(rl, tl, r::mask, i32, fregs)

  fun stdCheckLimit {maxAlloc, regfmls, regtys, return} = let
    val lab = checkLimit(maxAlloc, regtys)
    val (maskRegs, i32Regs, fregs) = maskList(regfmls, regtys, [], [], [])
  in
    clusterGcBlocks := 
       GCINFO{lab=ref lab, maskRegs=maskRegs, 
	      i32Regs=i32Regs, fRegs=fregs, ret=return} :: (!clusterGcBlocks)
  end

  fun knwCheckLimit {maxAlloc, regfmls, regtys, return} = let
    val lab = checkLimit(maxAlloc, regtys)
    val (maskRegs, i32Regs, fregs) = maskList(regfmls, regtys, [], [], [])
  in
    knownGcBlocks := 
      GCINFO{lab=ref lab, maskRegs=maskRegs, 
	     i32Regs=i32Regs, fRegs=fregs, ret=return} :: (!knownGcBlocks)
  end


  (* invoke GC.
   * A record of live floating point registers is created on the
   * heap and assigned to a pseudo register.
   *)
  val exhausted = T.CCR C.exhausted
  val gclinkreg = T.GPR C.gclinkreg
  val maskreg = T.GPR C.maskreg

  (* allocptr must always be in a registe *)
  val T.REG allocptrR = C.allocptr

  fun invokeGC regmap (GCINFO{lab, maskRegs, fRegs, i32Regs, ret}) = let
    fun assign(T.REG r, v) = T.MV(r, v)
      | assign(T.LOAD32(ea, region), v) = T.STORE32(ea, v, region)
      | assign _ = error "assign"

    fun mkRoot(T.REG r) = Const.Reg r
      | mkRoot(T.LOAD32(r as T.REG _, _)) = (checkStackPtr r; Const.Mem 0)
      | mkRoot(T.LOAD32(T.ADD(r, T.LI i), _)) = (checkStackPtr r; Const.Mem i)
      | mkRoot _ = error "root"

    fun callGC () = let
      val roots = map T.GPR (maskRegs @ i32Regs)
      val def = exhausted::gclinkreg:: roots
      val use = maskreg::roots
      val gcAddr = T.ADD (C.stackptr, T.LI MS.startgcOffset)

      fun hasPseudoRegs [] = false
        | hasPseudoRegs(T.REG r::regs) =  
	    r >= Cells.firstPseudoReg orelse hasPseudoRegs regs
	| hasPseudoRegs(_::regs) = hasPseudoRegs regs

      val regmaskVal = ConstType.REGLIST(map mkRoot maskRegs, regmap)
      val const = if hasPseudoRegs maskRegs 
	then T.CONST regmaskVal
	else T.LI(ConstType.valueOf regmaskVal)
    in
      (* Note that the live floating point registers are guaranteed
       * to be preserved across the call to GC
       *)
      emit(assign(C.maskreg, const));
      emit(T.CALL(T.LOAD32 (gcAddr, R.STACK), def, use))
    end

    fun gcReturn () = 
      (emit ret; 
       comp(T.ESCAPEBLOCK(map T.GPR maskRegs @ (exhausted :: dedicated))))
  in
    comp (T.ENTRYLABEL(!lab));
    case fRegs
     of [] => (callGC(); gcReturn())
      | _ => let
	  val k = length fRegs
	  val desc = LargeWord.toInt(D.makeDesc(k, D.tag_realdarray))
	  val baseR = Cells.newReg()

	  fun deposit([], _) = ()
	    | deposit(fpr::rest, i) = 
	       (emit(T.STORED (T.ADD (C.allocptr, T.LI i), fpr, R.STACK));
		deposit(rest,i+8))

	  fun restore([],_,_) = ()
	    | restore(T.FREG fpr::rest, base, i) = 
	       (emit(T.FMV(fpr, T.LOADD (T.ADD(base,T.LI i), R.STACK)));
		restore(rest, base, i+8))
	in
	   (* align allocation pointer *)
	   emit(T.MV(allocptrR, T.ORB(C.allocptr, T.LI 4)));
	   deposit(fRegs, 4);
	   emit(T.STORE32(C.allocptr, T.LI desc, R.STACK));
	   emit(T.STORE32(T.ADD(C.stackptr, T.LI MS.pseudoRegOffset),
			  T.ADD(C.allocptr, T.LI 4), R.STACK));
	   emit(T.MV(allocptrR, T.ADD(C.allocptr, T.LI(k * 8 + 4))));
	   callGC();
	   emit(T.MV(baseR, 
		     T.LOAD32(T.ADD(C.stackptr,T.LI MS.pseudoRegOffset), 
			      R.STACK)));
	   restore(fRegs, T.REG baseR, 0);
	   gcReturn()
	end
  end (* invokeGC *) 
    
  (* Called once at the end of compiling a cluster. 
   * Calls to invoke GC for known functions, and long jumps
   * (to the end of the module unit) for standard functions are generated. 
   *)
  fun emitLongJumpsToGCInvocation regmap = let
    fun find(info as GCINFO{lab as ref l, ...}) = let
      (* linear search to find the gc subroutine *)
      fun search [] = let
	    val label = Label.newLabel""
	  in
	    moduleGcBlocks := info :: !moduleGcBlocks;
	    lab := label;
	    (info, l)
	  end
	| search(info1::rest) = 
	  if equal(info, info1) then (info1, l) else search rest 
    in
      search(!moduleGcBlocks)
    end (*find*)

    fun collapse(gcCall::rest, merged) = let
	  val (info as GCINFO{lab as ref l,...},  label) = find gcCall
	  (* merge this with other calls to the same subroutine *)
	  fun merge [] = [(info, [label])]
	    | merge ((p as (GCINFO{lab as ref l1, ...}, labs))::rest) = 
	      if Label.id l1 = Label.id l then (info, label::labs)::rest
	      else p::merge rest
        in
	  collapse(rest, merge merged)
	end
      | collapse([], merged) = merged


    fun emitLongJumps(GCINFO{lab,maskRegs,i32Regs,fRegs,...},labs) = let
      val regRoots = map T.GPR (i32Regs @ maskRegs)
      val fregRoots = map T.FPR fRegs
      val liveOut = fregRoots @regRoots
      val l = !lab
    in
      app (fn lab => comp(T.DEFINELABEL lab)) labs;
      emit(T.JMP(T.LABEL(LE.LABEL(l)), [l]));
      comp(T.ESCAPEBLOCK(liveOut @ dedicated))
    end
  in
    (app (invokeGC regmap) (!knownGcBlocks)) before knownGcBlocks:=[];
    app emitLongJumps (collapse(!clusterGcBlocks, [])) 
    				before clusterGcBlocks:=[]
  end (*emitLongJumpsToGC*)

  fun emitInvokeGC regmap = 
    (app (invokeGC regmap) (!moduleGcBlocks)) before moduleGcBlocks:=[]
end




(*
 * $Log: callgc.sml,v $
 * Revision 1.7  1998/02/15 19:41:24  jhr
 *   Changed references to System.Tags to MachSpec.ObjDesc.
 *
 * Revision 1.6  1997/09/29 20:58:42  george
 *   Gc invocation code should have an edge from the ENTRY block
 *
 * Revision 1.5  1997/09/17  17:15:22  george
 *   included dedicated registers in ESCAPEBLOCK
 *
 * Revision 1.4  1997/09/12  10:13:02  george
 *   Checklimit must be done with unsigned comparisons
 *
 * Revision 1.3  1997/07/28  20:04:47  george
 *   Added support for regions
 *
 * Revision 1.2  1997/07/17  12:36:29  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)

(* callgc.sml -- gc calls and invocation. 
 * 
 * COPYRIGHT (c) 1999 Bell Labs
 *)

functor CallGC
  (structure Cells : CELLS
   structure C : CPSREGS where T.Region=CPSRegions
   structure MS: MACH_SPEC
   structure MLTreeComp : MLTREECOMP where T = C.T
   structure MkRecord : MK_RECORD where T = C.T
   ) : CALLGC =
struct
  structure T : MLTREE = MLTreeComp.T
  structure D = MS.ObjDesc
  structure LE = LabelExp
  structure R = CPSRegions
  structure W = Word
  structure S = SortedList

  val dtoi = LargeWord.toInt
  val emit = MLTreeComp.mlriscComp
  val comp = MLTreeComp.mltreeComp

  val skidPad = 4096			(* extra space in allocation space *)

  fun error msg = ErrorMsg.impossible ("CallGC." ^ msg)

  type t =
    {maxAlloc: int,
     regfmls:  T.mlrisc list,
     regtys: CPS.cty list,
     return: T.stm}

  datatype binding =
      Reg of int			(* register *)
    | Raw of				(* float + int32 record *)
       {reg: int,			(* pointer to record *)
	orig: T.mlrisc list}
    | Record of				(* bundle of boxed objects *)
       {reg: int,			(* pointer to record *)
	orig: binding list}
    | Mem of int
    | None

  fun mapOnto(f, [], tl) = tl
    | mapOnto(f, x::xs, tl) = f x::mapOnto(f, xs, tl)

  val sp = Cells.stackptrR
  val unit = 1				(* used to initialize registers *)

  fun stackEA 0 = T.REG sp
    | stackEA n = T.ADD(T.REG sp, T.LI n)

  fun storeToSp(n, e) = emit(T.STORE32(stackEA n, e, R.STACK))
  fun loadFromSp(r, n) = emit(T.MV(r, T.LOAD32(stackEA n, R.STACK)))

  fun set bindings = let
    fun isStackPtr sp = (sp = Cells.stackptrR)
    fun live(rexp, {regs, mem}) = 
      (case rexp
       of T.REG r => {regs=r::regs, mem=mem}
        | T.LOAD32(T.REG sp, _) => 
           if isStackPtr sp then {regs=regs, mem=0::mem} 
	   else error "set:LOAD32"
	| T.LOAD32(T.ADD(T.REG sp, T.LI i), _) => 
           if isStackPtr sp then {regs=regs, mem=i::mem} 
	   else error "set:ADD"
	| _ => error "set"
       (*esac*))
    val {regs, mem} = List.foldl live {regs=[], mem=[]} bindings
  in {regs=S.uniq regs, mem=S.uniq mem}
  end

  fun setToList{regs, mem} = mapOnto(Reg, regs, map Mem mem)

  fun difference({regs=r1, mem=m1}, {regs=r2, mem=m2}) = 
    {regs=S.difference(r1, r2),  mem=S.difference(m1, m2)}

  fun size{regs, mem} = length regs + length mem

  (* GcInfo encapsulates all the  information needed to generate
   * code to invoke gc 
   *)
  datatype gcInfo =			
      GCINFO of
        {known: bool,		(* known function ? *)
	 lab: Label.label ref,  (* labels to invoke for GC *)
	 boxed : T.rexp list,	(* locations with boxed objects *)
	 int32 : T.rexp list,	(* locations with int32 objects *)
	 float: T.fexp list,	(* locations with float objects *)
	 ret: T.stm}		(* how to return *)
    | MODULE of 
	{info: gcInfo,
	 addrs: Label.label list ref} (* addrs associated with long jump *)

  (* callee-save registers *)
  val calleesaves = List.take(C.miscregs, MS.numCalleeSaves)

  (* registers that are the roots of gc *)
  val allregs = (C.stdlink::C.stdclos::C.stdcont::C.stdarg::calleesaves)
  val allRoots = set allregs
  val aroot = hd(#regs allRoots)	(* There had better be at least one! *)
  val aRoot = Reg aroot

  (* gc info required for standard functions within the cluster *)
  val clusterGcBlocks = ref([]: gcInfo list)

  (* gc info required for known functions within the cluster *)
  val knownGcBlocks = ref([]: gcInfo list)

  local
    (* generate the checklimit, returning the label to 
     * branch to to invoke GC.
     *)
    fun checkLimit (maxAlloc) = let
      val lab = Label.newLabel ""
      
      fun assignCC(T.CC cc, v) = T.CCMV(cc, v)
	| assignCC(T.LOADCC(ea,region), v) = T.STORECC(ea, v, region)
	| assignCC _ = error "checkLimit.assign"

      fun gotoGC(cc) = emit(T.BCC(T.GTU, cc, lab))
      fun testLimit(allocR) = T.CMP(T.GTU, allocR, C.limitptr, T.LR)
    in
      if maxAlloc < skidPad then 
	(case C.exhausted 
	 of SOME cc => gotoGC(cc) 
	  | NONE => gotoGC(testLimit(C.allocptr))
	 (*esac*))
      else let
	  val shiftedAllocPtr = T.ADD(C.allocptr, T.LI(maxAlloc-4096))
	in
	  case C.exhausted
 	  of SOME cc => 
	      (emit(assignCC(cc, testLimit(shiftedAllocPtr)));
	       gotoGC(cc))
	   | NONE => gotoGC(testLimit(shiftedAllocPtr))
	  (*esac*)
	end;
      lab
    end

    fun maskList([], [], boxed, int32, float) = (boxed, int32, float)
      | maskList(T.GPR r::rl, t::tl, b, i, f) = 
        (case t
	  of CPS.INT32t => maskList(rl, tl, b, r::i, f)
	   | _ => maskList(rl, tl, r::b, i, f)
	(*esac*))
      | maskList(T.FPR r::rl, CPS.FLTt::tl, b, i, f) = 
	 maskList(rl, tl, b, i, r::f)
      | maskList _ = error "checkLimit.maskList"

    fun genGcInfo (clusterRef,known) {maxAlloc, regfmls, regtys, return} = let
      val (boxed, int32, float) = maskList(regfmls, regtys, [], [], [])
    in
      clusterRef :=
	 GCINFO{known=known,
		lab=ref (checkLimit(maxAlloc)),
		boxed=boxed,
		int32=int32,
		float=float,
		ret=return} :: (!clusterRef)
    end
  in
    val stdCheckLimit = genGcInfo (clusterGcBlocks, false)
    val knwCheckLimit = genGcInfo (knownGcBlocks, true)
  end (*local*)

  (* allocptr must always be in a register *)
  val T.REG allocptrR = C.allocptr

  fun invokeGC (external, regmap) gcInfo = let
    val {known, boxed, int32, float, ret, lab} =
      (case gcInfo
	of GCINFO info => info
         | MODULE {info=GCINFO info, ...} => info
	 | _ => error "invokeGC:gcInfo"
      (*esac*))

    val liveBoxed = set boxed
    val liveRegs = difference(liveBoxed, allRoots)
    val gcRoots = difference(allRoots, liveBoxed)

    fun copy([], []) = ()
      | copy(dst, src) = emit(T.COPY(dst, src))

    fun assign(dst, src) = 
      (case (dst, src)
       of (Reg i, None) => emit(T.MV(i, T.LI unit))
	| (Mem i, None) => emit(T.STORE32(stackEA i, T.LI unit, R.STACK))
	| (Reg i, Mem j) => loadFromSp(i, j)
	| (Mem i, Reg j) => emit(T.STORE32(stackEA i, T.REG j, R.STACK))
	| (Reg i, Reg j) => emit(T.COPY([i],[j]))
	| (Mem i, Mem j) => storeToSp(i, T.LOAD32(stackEA j, R.STACK))
	| _ => error "assign"
      (*esac*))

    (* invariant: number of live regs < number of gcRoots *)
    fun assignGcRoots(liveRegs, raw, record, gcRoots) = let
      val {regs=liveR, mem=liveM} = liveRegs
      val {regs=gcR, mem=gcM} = gcRoots
      val liveMem = map Mem liveM
      val gcMem = map Mem gcM

      fun doMem(liveRoots, gcRoots, tbl, dst, src, undo) = let
	fun move(src::live, dst::gc, tbl) = 
	     (assign(dst, src); move(live, gc, {loc=dst, value=src}::tbl))
	  | move([], [], tbl) = (undo, tbl)
	  | move([], dst::gc, tbl) = (assign(dst, None); move([], gc, tbl))
      in
	copy(dst, src); move(liveRoots, gcRoots, tbl)
      end

      fun doRecord(liveMem, gcRoots, tbl, dst, src, undo) = 
	(case record
	 of NONE => doMem(liveMem, gcRoots, tbl, dst, src, undo)
	  | SOME(recd as Record{reg, ...}) => 
	     (case gcRoots
	       of Reg r::rest =>
		   doMem(liveMem, rest, {loc=Reg r, value=recd}::tbl, 
			 r::dst, reg::src, undo)
		| Mem i::rest => 
		   (emit(T.STORE32(stackEA i, T.REG reg, R.STACK));
		    doMem(liveMem, rest, 
			  {loc=Mem i, value=recd}::tbl, dst, src, undo))
	     (*esac*))
	(*esac*))

      fun doRaw(liveMem, gcRoots, dst, src, undo) = 
       (case raw 
	of NONE => doRecord(liveMem, gcRoots, [], dst, src, undo)
	 | SOME(rw as Raw{reg, ...}) => 
	   (case gcRoots
	    of Reg r::rest => 
		doRecord(liveMem, rest, 
			 [{loc=Reg r, value=rw}], r::dst, reg::src, undo)
	     | Mem i::rest => 
		(emit(T.STORE32(stackEA i, T.REG reg, R.STACK));
		 doRecord(liveMem, rest, 
			  [{loc=Mem i, value=rw}], dst, src, undo))
	     | _ => error "doRaw"
	  (*esac*))
       (*esac*))

      fun copyRegs(r::liveR, g::gcR, dst, src) = 
	    copyRegs(liveR, gcR, g::dst, r::src)
	| copyRegs([], [], dst, src) = 
	    doRaw(liveMem, gcMem, dst, src, (src,dst))
	| copyRegs([], gcR, dst, src) = 
	    doRaw([], mapOnto(Reg, gcR, gcMem), dst, src, (src,dst))
	| copyRegs(liveR, [], dst, src) = 
	    doRaw(mapOnto(Reg, liveR, liveMem), gcMem, dst, src, (src,dst))
    in 
      copyRegs(liveR, gcR, [], [])
    end (* assignGcRoots *)

    (* Move liveregs into gcroots. 
     * We are conservative (read lazy) about memory disambiguation 
     * information and mark all regions as RW_MEM, which will mean
     * that none of these memory operations can be reordered.
     *)
    fun zip() = let
      fun mkRaw64Array() = let
	val len = length float + (length int32 + 1) div 2
	val desc = dtoi(D.makeDesc(len + len, D.tag_raw64))
	val ans = Cells.newReg()
	fun storefields() = let
	  fun storefloat(f, offset) =
	    (emit(T.STORED(T.ADD(C.allocptr, T.LI offset), f, R.RW_MEM));
	     offset+8)
	  fun storeint32(i32, offset) =
	    (emit(T.STORE32(T.ADD(C.allocptr, T.LI offset), i32, R.RW_MEM));
	     offset+4)
	in 
	  List.foldl storeint32 (List.foldl storefloat 4 float) int32
	end (*storefields*)
      in
	emit(T.MV(allocptrR, T.ORB(C.allocptr, T.LI 4))); (* align *)
	emit(T.STORE32(C.allocptr, T.LI desc, R.RW_MEM));
	emit(T.MV(ans, T.ADD(C.allocptr, T.LI 4)));
	storefields();
	emit(T.MV(allocptrR, T.ADD(C.allocptr, T.LI(len * 8 + 4))));
	Raw{reg=ans, orig=mapOnto(T.FPR, float, map T.GPR int32)}
      end (* mkRaw64Array *)

      fun mkRecord(fields) = let
	val len = length fields
	val desc = T.LI(dtoi(D.makeDesc(length fields, D.tag_record)))
	val ans = Cells.newReg()
	fun getReg boxed = let
	  fun f(Reg r) =  r
	    | f(Raw{reg, ...}) = reg
	    | f(Mem i) = let
		val tmp = Cells.newReg()
	      in loadFromSp(tmp, i); tmp
	      end
	    | f _ = error  "mkRecord.getReg.f"
	  val offp0 = CPS.OFFp 0
	in (T.REG(f boxed), offp0)
	end
	val vl = map getReg fields
      in 
	MkRecord.record{desc=desc, fields=vl, ans=ans, mem=R.RW_MEM, hp=0};
	emit(T.MV(allocptrR, T.ADD(C.allocptr, T.LI (len*4+4))));
	Record{reg=ans, orig=fields}
      end (* mkRecord *)

      val raw = 
	(case (int32, float) 
         of ([], []) => NONE 
          | _ => SOME(mkRaw64Array())
	 (*esac*))

      val nLiveRegs = size(liveRegs) + (case raw of NONE =>  0 | _ => 1)
      val nGcRoots = size(gcRoots)
    in
      if nLiveRegs <= nGcRoots then 
	assignGcRoots(liveRegs, raw, NONE, gcRoots)
      else if nGcRoots = 0 then let
	  val live = setToList liveRegs
	  val empty = {regs=[], mem=[]}
	  val recd = 
	    (case raw 
	     of NONE => mkRecord(aRoot::live)
	      | SOME rw => mkRecord(aRoot::rw::live)
	    (*esac*))
	in assignGcRoots(empty, NONE, SOME recd, {regs=[aroot],mem=[]})
	end
      else let
	  fun split(0, regs, mem, raw, acc) = 
	       (mkRecord acc, {regs=regs, mem=mem}, raw)
	    | split(n, r::regs, mem, raw, acc) = 
	       split(n-1, regs, mem, raw, Reg r::acc)
	    | split(n, [], m::mem, raw, acc) = 
	       split(n-1, [], mem, raw, Mem m::acc)
	    | split(n, [], [], SOME raw, acc) = 
	       split(n-1, [], [], NONE, raw::acc)
	    | split(n, [], [], NONE, acc) = error "zip.split"
	  val {regs, mem} = liveRegs
	  val (recd, live, raw) = 
	    split(nLiveRegs-nGcRoots+1, regs, mem, raw, [])
	in assignGcRoots(live, raw, SOME recd, gcRoots)
	end
    end (*zip *)

    fun unzip(undo, tbl) = let
      fun move {loc, value=Raw{orig, ...}} = let
	   val tmp = Cells.newReg()
	   fun srcAddr i = T.ADD(T.REG tmp, T.LI i)
	   fun unbundle(fexp, offset) = 
	     (case fexp
	      of T.FPR(T.FREG f) => 
		  (emit(T.FMV(f, T.LOADD(srcAddr offset, R.RO_MEM))); 
		   offset+8)
	       | T.FPR(T.LOADD(ea, _)) =>
		  (emit(T.STORED(ea, T.LOADD(srcAddr offset, R.RO_MEM), R.RO_MEM));
		   offset+8)
	       | T.GPR(T.REG r) => 
		  (emit(T.MV(r, T.LOAD32(srcAddr offset, R.RO_MEM))); 
		   offset+4)
	      (*esac*))
	  in 
	    assign(Reg tmp, loc);  List.foldl unbundle 0 orig;  ()
	  end
	| move {loc, value=Record{orig, ...}} = let
	    val tmp = Cells.newReg()
	    fun srcValue i = T.LOAD32(T.ADD(T.REG tmp, T.LI i), R.RO_MEM)
	    fun unbundle(elem, offset) = 
	      (case elem
	       of Raw{reg, ...} =>
		   (emit(T.MV(reg, srcValue offset));  
		    move{loc=Reg reg, value=elem};
		    offset+4)
	        | Reg r => 
		   (emit(T.MV(r, srcValue(offset))); offset+4)
		| Mem m => 
		   (emit(T.STORE32(stackEA m, srcValue offset, R.RO_MEM));
		    offset+4) 
	       (*esac*))
	  in 
	    assign(Reg tmp, loc);  List.foldl unbundle 0 orig; ()
	  end
	| move{loc, value} = assign(value, loc)
    in copy undo; app move tbl
    end (* unzip *)

    fun callGc() = let
      val roots = map T.GPR allregs
      val def = case C.exhausted of NONE => roots | SOME cc => T.CCR cc::roots
      val use = roots
      val gcAddr = T.ADD (C.stackptr, T.LI MS.startgcOffset)
    in
      emit(T.CALL(T.LOAD32(gcAddr, R.STACK), def, use));
      if known then let			(* recompute base address *)
	  val returnLab = Label.newLabel ""
	  fun assignBasePtr baseptr = let
	    val baseExp =
	      T.ADD(C.gcLink, 
		    T.LABEL(LE.MINUS(LE.CONST MS.constBaseRegOffset, 
				     LE.LABEL returnLab)))
	  in
	    case baseptr
	    of T.REG bpt => T.MV(bpt, baseExp)
	     | T.LOAD32(ea, mem) => T.STORE32(ea, baseExp, mem)
	     | _  => error "callGc: baseptr"
          end
	in
	  comp(T.DEFINELABEL returnLab);
	  emit(assignBasePtr(C.baseptr))
	end
      else ()
    end
    fun gcReturn () = let
      val live' = map T.GPR allregs
      val live = case C.exhausted of NONE => live' | SOME cc => T.CCR cc::live'
    in emit ret; comp(T.ESCAPEBLOCK live)
    end
  in 
    comp ((if external then T.ENTRYLABEL else T.DEFINELABEL)(!lab));
    unzip(zip() before callGc());
    gcReturn()
  end (*invokeGC*)

  val moduleGcBlocks = ref ([]: gcInfo list)

  (* Called once at the end of compiling a cluster. *)
  (* Generates long jumps to the end of the module unit for
   * standard functions, and directly invokes GC for known functions.
   *)
  fun emitLongJumpsToGCInvocation regmap = let
    (* GC code can be shared if the calling convention is the same *)
    fun equal
	 (GCINFO{boxed=b1, int32=i1, float=f1, ret=T.JMP(ret1, _), ...},
	  GCINFO{boxed=b2, int32=i2, float=f2, ret=T.JMP(ret2, _), ...}) = let
	 fun eqEA(T.REG r1, T.REG r2) = (r1 = r2)
	   | eqEA(T.ADD(T.REG r1, T.LI i), T.ADD(T.REG r2, T.LI j)) =
	       r1=r2 andalso i=j		
	   | eqEA _ = false
	 fun eqR(T.REG r1, T.REG r2) = (r1 = r2)
	   | eqR(T.LOAD32(ea1, _), T.LOAD32(ea2, _)) = eqEA(ea1, ea2)
	   | eqR _ = false

	  fun eqF(T.FREG f1, T.FREG f2) = (f1=f2)
	    | eqF(T.LOADD(ea1, _), T.LOADD(ea2, _)) = eqEA(ea1, ea2)
	    | eqF _ = false

	  val eqRexp = ListPair.all eqR
	in 
	  eqRexp (b1, b2) andalso  eqRexp (ret1::i1, ret2::i2) 
	    andalso ListPair.all eqF (f1, f2)
	end 
      | equal _ = false

    fun find(info as GCINFO{lab as ref l, ...}) = let
      (* linear search to find the gc subroutine *)
      fun search((info1 as MODULE{info=info', addrs})::rest) = 
	   if equal(info, info') then addrs := l::(!addrs) else search rest
	| search [] = let
	    val label = Label.newLabel""
	  in
	    lab := label;
	    moduleGcBlocks := MODULE{info=info, addrs=ref[l]}:: !moduleGcBlocks
	  end
    in search(!moduleGcBlocks)
    end (*find*)

    fun longJumps(MODULE{info, addrs=ref []}) = ()
      | longJumps(MODULE{info=GCINFO{lab,boxed,int32,float, ...}, addrs}) = let
      val regRoots = map T.GPR (int32 @ boxed)
      val fregRoots = map T.FPR float
      val liveOut = fregRoots @ regRoots
      val l = !lab
    in
      app (fn lab => comp(T.DEFINELABEL lab)) (!addrs) before addrs:=[];
      emit(T.JMP(T.LABEL(LE.LABEL(l)), [l]));
      comp(T.ESCAPEBLOCK liveOut)
    end
  in
    (app find (!clusterGcBlocks)) before clusterGcBlocks := [];
    app longJumps (!moduleGcBlocks);
    (app (invokeGC (false,regmap)) (!knownGcBlocks)) before knownGcBlocks:=[]
  end (*emitLongJumpsToGC*)

  (* module specific gc invocation code *)
  fun emitModuleGC regmap = 
    (app (invokeGC (true,regmap)) (!moduleGcBlocks)) before moduleGcBlocks:=[]

end


(* callgc.sml -- gc calls and invocation. 
 * 
 * COPYRIGHT (c) 1999 Bell Labs
 *)

functor CallGC
  (structure Cells : CELLS
   structure C : CPSREGS where T.Region=CPSRegions
   structure MS: MACH_SPEC
   structure MkRecord : MK_RECORD where T = C.T
   ) : CALLGC =
struct
  structure T : MLTREE = C.T
  structure D = MS.ObjDesc
  structure LE = LabelExp
  structure R = CPSRegions
  structure W = Word
  structure S = SortedList
  structure St = T.Stream

  val dtoi = LargeWord.toInt

  val skidPad = 4096			(* extra space in allocation space *)

  type emitter = (T.stm,int Intmap.intmap) T.stream

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

  fun stackEA 0 = T.REG(32,sp)
    | stackEA n = T.ADD(32,T.REG(32,sp), T.LI n)

  fun storeToSp(n, e) = T.STORE(32, stackEA n, e, R.stack)
  fun loadFromSp(r, n) = T.MV(32, r, T.LOAD(32, stackEA n, R.stack))

  fun set bindings = let
    fun isStackPtr sp = (sp = Cells.stackptrR)
    fun live(rexp, {regs, mem}) = 
      (case rexp
       of T.REG(_,r) => {regs=r::regs, mem=mem}
        | T.LOAD(32,T.REG(_,sp), _) => 
           if isStackPtr sp then {regs=regs, mem=0::mem} 
	   else error "set:LOAD32"
	| T.LOAD(32,T.ADD(_,T.REG(_,sp), T.LI i), _) => 
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
	 regfmls: T.mlrisc list,(* all live registers *)
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
    val unlikely = BasicAnnotations.BRANCH_PROB 0
    val gcCmp    = if C.signedGCTest then T.GT else T.GTU
    fun testLimit(allocR) = T.CMP(32, gcCmp, allocR, C.limitptr)
    val normalTestLimit = testLimit(C.allocptr) 
    fun checkLimit emit (maxAlloc) = let
      val lab = Label.newLabel ""
      
      fun assignCC(T.CC cc, v) = T.CCMV(cc, v)
	(*| assignCC(T.LOADCC(ea,region), v) = T.STORECC(ea, v, region)*)
	| assignCC _ = error "checkLimit.assign"

      fun gotoGC(cc) = emit(T.ANNOTATION(T.BCC(gcCmp, cc, lab),unlikely))
    in
      if maxAlloc < skidPad then 
	(case C.exhausted 
	 of SOME cc => gotoGC(cc) 
	  | NONE => gotoGC(normalTestLimit)
	 (*esac*))
      else let
	  val shiftedAllocPtr = T.ADD(32, C.allocptr, T.LI(maxAlloc-4096))
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
           | CPS.FLTt => error "checkLimit.maskList: T.GPR"
	   | _ => maskList(rl, tl, r::b, i, f)
	(*esac*))
      | maskList(T.FPR r::rl, CPS.FLTt::tl, b, i, f) = 
	 maskList(rl, tl, b, i, r::f)
      | maskList _ = error "checkLimit.maskList"

    fun genGcInfo (clusterRef,known) (St.STREAM{emit,...} : emitter)
                  {maxAlloc, regfmls, regtys, return} = let
      val (boxed, int32, float) = maskList(regfmls, regtys, [], [], [])
    in
      clusterRef :=
	 GCINFO{known=known,
		lab=ref (checkLimit emit (maxAlloc)),
		boxed=boxed,
		int32=int32,
		float=float,
		regfmls=regfmls,
		ret=return} :: (!clusterRef)
    end
  in
    val stdCheckLimit = genGcInfo (clusterGcBlocks, false)
    val knwCheckLimit = genGcInfo (knownGcBlocks, true)
  end (*local*)

  (* allocptr must always be in a register *)
  val T.REG(_,allocptrR) = C.allocptr

  fun invokeGC 
      (emitter as
       St.STREAM{emit,defineLabel,entryLabel,exitBlock,annotation,...},
       external) gcInfo = let
    val {known, boxed, int32, float, regfmls, ret, lab} =
      (case gcInfo
	of GCINFO info => info
         | MODULE {info=GCINFO info, ...} => info
	 | _ => error "invokeGC:gcInfo"
      (*esac*))

    val liveBoxed = set boxed
    val liveRegs = difference(liveBoxed, allRoots)
    val gcRoots = difference(allRoots, liveBoxed)

    fun copy([], []) = ()
      | copy(dst, src) = emit(T.COPY(32, dst, src))

    fun assign(dst, src) = 
      (case (dst, src)
       of (Reg i, None) => emit(T.MV(32, i, T.LI unit))
	| (Mem i, None) => emit(T.STORE(32, stackEA i, T.LI unit, R.stack))
	| (Reg i, Mem j) => emit(loadFromSp(i, j))
	| (Mem i, Reg j) => emit(T.STORE(32, stackEA i, T.REG(32, j), R.stack))
	| (Reg i, Reg j) => emit(T.COPY(32, [i],[j]))
	| (Mem i, Mem j) => emit(storeToSp(i, T.LOAD(32, stackEA j, R.stack)))
	| _ => error "assign"
      (*esac*))

    (* invariant: number of live regs < number of gcRoots *)
    fun assignGcRoots(liveRegs, raw, record, gcRoots) = let
      val {regs=liveR, mem=liveM} = liveRegs
      val {regs=gcR, mem=gcM} = gcRoots
      val liveMem = map Mem liveM
      val gcMem = map Mem gcM

      fun doMem(liveRoots, gcRoots, tbl, dst, src) = let
	fun move(src::live, dst::gc, tbl) = 
	     (assign(dst, src); move(live, gc, {loc=dst, value=src}::tbl))
	  | move([], [], tbl) = tbl
	  | move([], dst::gc, tbl) = (assign(dst, None); move([], gc, tbl))
      in
	copy(dst, src);
	(dst, src, move(liveRoots, gcRoots, tbl))
      end

      fun doRecord(live, gcRoots, tbl, dst, src) =
	(case record
	 of NONE => doMem(live, gcRoots, tbl, dst, src)
	  | SOME(recd as Record{reg, ...}) => 
	     (case gcRoots
	       of Reg r::rest =>
		  (emit(T.COPY(32, [r], [reg]));
		   doMem(live, rest, {loc=Reg r, value=recd}::tbl, dst, src))
		| Mem i::rest => 
		   (emit(T.STORE(32, stackEA i, T.REG(32,reg), R.stack));
		    doMem(live, rest, {loc=Mem i, value=recd}::tbl, dst, src))
	     (*esac*))
	(*esac*))

      fun doRaw(live, gcRoots, dst, src) = 
       (case raw 
	of NONE => doRecord(live, gcRoots, [], dst, src)
	 | SOME(rw as Raw{reg, ...}) => 
	   (case gcRoots
	    of Reg r::rest =>
	        (emit(T.COPY(32, [r], [reg]));
 	 	 doRecord(live, rest, [{loc=Reg r, value=rw}], dst, src))
	     | Mem i::rest => 
		(emit(T.STORE(32, stackEA i, T.REG(32,reg), R.stack));
		 doRecord(live, rest, [{loc=Mem i, value=rw}], dst, src))
	     | _ => error "doRaw"
	  (*esac*))
       (*esac*))

      fun copyRegs(r::liveR, g::gcR, dst, src) = 
	   copyRegs(liveR, gcR, g::dst, r::src)
	| copyRegs(liveR, gcR, dst, src) = let
	   val liveRegs = mapOnto(Reg, liveR, liveMem)
	   val gcRoots = mapOnto(Reg, gcR, gcMem)
	  in doRaw(liveRegs, gcRoots, dst, src)
	  end
    in 
      copyRegs(liveR, gcR, [], [])
    end (* assignGcRoots *)

    (* Move liveregs into gcroots. 
     * We are conservative (read lazy) about memory disambiguation 
     * information and mark all regions as RW_MEM, which will mean
     * that none of these memory operations can be reordered.
     * Probably doesn't matter anyway.
     *)
    fun zip() = let
      fun mkRaw64Array() = let
	fun storefields() = let
	  fun storefloat(f, offset) =
	    (emit(T.FSTORE(64,T.ADD(32, C.allocptr, T.LI offset), f, R.memory));
	     offset+8)
	  fun storeint32(i32, offset) =
	    (emit(T.STORE(32,T.ADD(32,C.allocptr, T.LI offset), i32, R.memory));
	     offset+4)
	in 
	  List.foldl storeint32 (List.foldl storefloat 4 float) int32
	end (*storefields*)
	val len = length float + (length int32 + 1) div 2
	val desc = dtoi(D.makeDesc(len + len, D.tag_raw64))
	val ans = Cells.newReg()
      in
	emit(T.MV(32, allocptrR, T.ORB(32, C.allocptr, T.LI 4))); (* align *)
	emit(T.STORE(32, C.allocptr, T.LI desc, R.memory));
	emit(T.MV(32, ans, T.ADD(32, C.allocptr, T.LI 4)));
	storefields();
	emit(T.MV(32, allocptrR, T.ADD(32, C.allocptr, T.LI(len * 8 + 4))));
	Raw{reg=ans, orig=mapOnto(T.FPR, float, map T.GPR int32)}
      end (* mkRaw64Array *)

      fun mkRecord(fields) = let
	fun getReg boxed = let
	  fun f(Reg r) =  r
	    | f(Raw{reg, ...}) = reg
	    | f(Mem i) = let
		val tmp = Cells.newReg()
	      in emit(loadFromSp(tmp, i)); tmp
	      end
	    | f _ = error  "mkRecord.getReg.f"
	  val offp0 = CPS.OFFp 0
	in (T.REG(32, f boxed), offp0)
	end
	val vl = map getReg fields
	val len = length fields
	val desc = T.LI(dtoi(D.makeDesc(len, D.tag_record)))
	val ans = Cells.newReg()
      in 
	MkRecord.record{desc=desc, fields=vl, ans=ans, mem=R.memory, 
                        hp=0, emit=emit};
	emit(T.MV(32, allocptrR, T.ADD(32, C.allocptr, T.LI (len*4+4))));
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
	     of NONE => mkRecord(rev(aRoot::live))
	      | SOME rw => mkRecord(rev(aRoot::rw::live))
	    (*esac*))
	in assignGcRoots(empty, NONE, SOME recd, {regs=[aroot],mem=[]})
	end
      else let (* nLiveRegs > nGcRoots *)
	  fun split(0, regs, mem, raw, fields) = 
	      (fields, {regs=regs,mem=mem}, raw)
	    | split(n, r::regs, mem, raw, fields) = 
	       split(n-1, regs, mem, raw, Reg r::fields)
	    | split(n, regs, m::mem, raw, fields) = 
	       split(n-1, regs, mem, raw, Mem m::fields)
	    | split(n, [], [], SOME raw, fields) = 
	       split(n-1, [], [], NONE, raw::fields)
	    | split(n, [], [], NONE, _) = error "zip.split"

	  val {regs, mem} = liveRegs
	  val (fields, live, raw) = 
	    split(nLiveRegs-nGcRoots+1, regs, mem, raw, [])
	in assignGcRoots(live, raw, SOME(mkRecord fields), gcRoots)
	end
    end (*zip *)

    fun unzip(dst, src, tbl) = let
      fun move {loc, value=Raw{orig, ...}} = let
	   val tmp = Cells.newReg()
	   fun srcAddr i = T.ADD(32, T.REG(32, tmp), T.LI i)
	   fun unbundle(fexp, offset) = 
	     (case fexp
	      of T.FPR(T.FREG(64, f)) => 
		  (emit(T.FMV(64, f, T.FLOAD(64, srcAddr offset, R.readonly))); 
		   offset+8)
	       | T.FPR(T.FLOAD(64,ea, _)) =>
		  (emit(T.FSTORE(64,ea,
                           T.FLOAD(64,srcAddr offset, R.readonly), R.readonly));
		   offset+8)
	       | T.GPR(T.REG(32, r)) => 
		  (emit(T.MV(32, r, T.LOAD(32, srcAddr offset, R.readonly))); 
		   offset+4)
	      (*esac*))
	  in 
	    assign(Reg tmp, loc);  List.foldl unbundle 0 orig;  ()
	  end
	| move {loc, value=Record{orig, ...}} = let
	    val tmp = Cells.newReg()
	    fun srcValue i = T.LOAD(32,T.ADD(32,T.REG(32,tmp),T.LI i),R.readonly)
	    fun unbundle(elem, offset) = 
	      (case elem
	       of Raw{reg, ...} => let
		    val tmp = Cells.newReg()
		  in
		   (emit(T.MV(32, tmp, srcValue offset));  
		    move{loc=Reg tmp, value=elem};
		    offset+4)
		  end
	        | Reg r => 
		   (emit(T.MV(32, r, srcValue(offset))); offset+4)
		| Mem m => 
		   (emit(T.STORE(32, stackEA m, srcValue offset, R.readonly));
		    offset+4) 
	       (*esac*))
	  in 
	    assign(Reg tmp, loc);  List.foldl unbundle 0 orig; ()
	  end
	| move{loc, value} = assign(value, loc)
    in 
      app move tbl;  copy(src, dst)
    end (* unzip *)

    fun callGc() = let
      val roots = map T.GPR allregs
      val def = case C.exhausted of NONE => roots | SOME cc => T.CCR cc::roots
      val use = roots
      val gcAddr = T.ADD (32, C.stackptr, T.LI MS.startgcOffset)
    in
      annotation(BasicAnnotations.CALLGC);
      emit(T.CALL(T.LOAD(32, gcAddr, R.stack), def, use, R.stack));
      if known then let			(* recompute base address *)
	  val returnLab = Label.newLabel ""
	  fun assignBasePtr baseptr = let
	    val baseExp =
	      T.ADD(32, C.gcLink, 
		    T.LABEL(LE.MINUS(LE.CONST MS.constBaseRegOffset, 
				     LE.LABEL returnLab)))
	  in
	    case baseptr
	    of T.REG(ty, bpt) => T.MV(ty, bpt, baseExp)
	     | T.LOAD(ty, ea, mem) => T.STORE(ty, ea, baseExp, mem)
	     | _  => error "callGc: baseptr"
          end
	in
	  defineLabel returnLab;
	  emit(assignBasePtr(C.baseptr))
	end
      else ()
    end
    fun gcReturn () = let
      val live = case C.exhausted of NONE => regfmls | SOME cc => T.CCR cc::regfmls
    in emit ret; exitBlock live
    end
  in 
    (if external then entryLabel else defineLabel)(!lab);
    unzip(zip() before callGc());
    gcReturn()
  end (*invokeGC*)

  val moduleGcBlocks = ref ([]: gcInfo list)

  (* Called once at the end of compiling a cluster. *)
  (* Generates long jumps to the end of the module unit for
   * standard functions, and directly invokes GC for known functions.
   *)
  fun emitLongJumpsToGCInvocation 
       (emitter as St.STREAM{emit,defineLabel,entryLabel,exitBlock,...}) = let
    (* GC code can be shared if the calling convention is the same *)
    fun equal
	 (GCINFO{boxed=b1, int32=i1, float=f1, ret=T.JMP(ret1, _), ...},
	  GCINFO{boxed=b2, int32=i2, float=f2, ret=T.JMP(ret2, _), ...}) = let
	 fun eqEA(T.REG(_,r1), T.REG(_,r2)) = (r1 = r2)
	   | eqEA(T.ADD(_,T.REG(_,r1),T.LI i), T.ADD(_,T.REG(_,r2),T.LI j)) =
	       r1=r2 andalso i=j		
	   | eqEA _ = false
	 fun eqR(T.REG(_,r1), T.REG(_, r2)) = (r1 = r2)
	   | eqR(T.LOAD(32,ea1, _), T.LOAD(32,ea2, _)) = eqEA(ea1, ea2)
	   | eqR _ = false

	  fun eqF(T.FREG(_,f1), T.FREG(_,f2)) = (f1=f2)
	    | eqF(T.FLOAD(64,ea1, _), T.FLOAD(64,ea2, _)) = eqEA(ea1, ea2)
	    | eqF _ = false

          fun all pred = let
            fun allp (a::r1, b::r2) = pred(a,b) andalso (allp (r1, r2))
              | allp ([], []) = true
              | allp _ = false
          in allp 
          end

          val eqRexp = all eqR
	in 
	  eqRexp (b1, b2) andalso  eqRexp (ret1::i1, ret2::i2) 
	    andalso all eqF (f1, f2)
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
      app defineLabel (!addrs) before addrs:=[];
      emit(T.JMP(T.LABEL(LE.LABEL(l)), [l]));
      exitBlock liveOut
    end
  in
    (app find (!clusterGcBlocks)) before clusterGcBlocks := [];
    app longJumps (!moduleGcBlocks);
    (app (invokeGC (emitter,false)) (!knownGcBlocks)) before knownGcBlocks:=[]
  end (*emitLongJumpsToGC*)

  (* module specific gc invocation code *)
  fun emitModuleGC stream = 
    (app (invokeGC (stream,true)) (!moduleGcBlocks)) before moduleGcBlocks:=[]

  fun init() = (clusterGcBlocks := []; 
                knownGcBlocks := []; 
                moduleGcBlocks := [])

end


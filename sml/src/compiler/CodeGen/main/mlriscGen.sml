(* mlriscGen.sml --- translate CPS to MLRISC.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature MLRISCGEN = sig
  val codegen : 
    CPS.function list * (CPS.lvar -> (int * int)) * ErrorMsg.complainer -> unit
end

functor MLRiscGen
 (  structure MachineSpec: MACH_SPEC
    structure PseudoOp   : SMLNJ_PSEUDO_OP_TYPE
    structure C          : CPSREGS
       where type T.Constant.const = SMLNJConstant.const
       where T.Region = CPSRegions
       and T.BNames = FunctionNames 
       and T.PseudoOp = PseudoOp
    structure Cells	: CELLS
    structure MLTreeComp : MLTREECOMP 
       where T = C.T
    structure Flowgen : FLOWGRAPH_GEN
       where T = MLTreeComp.T
         and I = MLTreeComp.I
         and S = MLTreeComp.S
  ): MLRISCGEN =
struct
  structure M : MLTREE = C.T
  structure P = CPS.P
  structure LE = LabelExp
  structure R = CPSRegions
  structure CG = Control.CG
  structure MS = MachineSpec

  structure D = MS.ObjDesc
  val dtoi = LargeWord.toInt	(* convert object descriptor to int *)

  structure ArgP = 
    ArgPassing(structure Cells=Cells
	       structure C=C
	       structure MS=MachineSpec)

  structure Frag = Frag(M)

  structure MemAliasing = MemAliasing(Cells)

  structure MkRecord = MkRecord(C)

  structure CallGc = 
    CallGC(structure Cells=Cells
	   structure MS=MachineSpec
	   structure C=C
           structure MkRecord = MkRecord
           )

  fun error msg = ErrorMsg.impossible ("MLRiscGen." ^ msg)

  val pty = (* C.pointerSize *) 32
  val ity = (* C.intSize   *) 32
  val fty = (* C.floatSize *) 64

  val newReg = Cells.newReg
  val newFreg = Cells.newFreg

  val M.REG(_,allocptrR) = C.allocptr

  val dedicated' = 
    map (fn r => M.GPR(M.REG(ity,r))) C.dedicatedR @ 
    map (fn f => M.FPR(M.FREG(fty,f))) C.dedicatedF
  val dedicated = 
    case C.exhausted of NONE => dedicated' | SOME cc => M.CCR cc :: dedicated'


  fun codegen(funcs : CPS.function list, limits:CPS.lvar -> (int*int), err) = let
    val {mlriscComp=emit,mltreeComp=comp,...} = 
          MLTreeComp.selectInstructions(Flowgen.newStream())
    val maxAlloc  = #1 o limits
    val instructionCount = #2 o limits 

    (* labelTbl: mapping of function names (CPS.lvars) to labels *)
    exception LabelBind and TypTbl
    val labelTbl : Label.label Intmap.intmap = Intmap.new(32, LabelBind)
    val functionLabel = Intmap.map labelTbl
    val addLabelTbl = Intmap.add labelTbl

    val typTbl  : CPS.cty Intmap.intmap = Intmap.new(32, TypTbl)
    val addTypBinding = Intmap.add typTbl
    val typmap = Intmap.map typTbl

    fun mkGlobalTables(fk, f, _, _, _) =
      (addLabelTbl (f, Label.newLabel(Int.toString f));
       case fk
	of CPS.CONT => addTypBinding(f, CPS.CNTt)
         | _ => addTypBinding(f, CPS.BOGt)
       (*esac*))

    fun genCluster(cluster) = let
      val _ = if !Control.debugging then app PPCps.printcps0 cluster else () 

      val sizeOfCluster = length cluster

      (* per-cluster tables *)
      exception RegMap and GenTbl 
      (* genTbl -- is used to retrieve the parameter passing 
       * conventions once a function has been compiled.
       *)
      val genTbl : Frag.frag Intmap.intmap = Intmap.new(sizeOfCluster, GenTbl)

      (* {fp,gp}RegTbl -- mapping of lvars to registers  *)
      val fpRegTbl : M.fexp Intmap.intmap = Intmap.new(2, RegMap)
      val gpRegTbl : M.rexp Intmap.intmap = Intmap.new(32, RegMap)
      fun clearTables() =(Intmap.clear fpRegTbl; Intmap.clear gpRegTbl)
      val addExpBinding = Intmap.add gpRegTbl
      fun addRegBinding(x,r) = addExpBinding(x,M.REG(ity,r))
      val addFregBinding = Intmap.add fpRegTbl

      val gcTest = M.CMP(32, if C.signedGCTest then M.GT else M.GTU, 
                             C.allocptr, C.limitptr)
  
      val treeify = CpsTreeify.usage cluster

      (* memDisambiguation uses the new register counters, 
       * so this must be reset here.
       *)
      val _ = Cells.reset()
      val regmap = Cells.regmap()

      val memDisambig = MemAliasing.analyze(cluster) 

      fun pi(x as ref(R.PT.TOP _),_) = x
        | pi(x as ref(R.PT.NAMED _),_) = x
        | pi(x,i) = R.PT.pi(x,i)

      fun getRegion(CPS.VAR v, i) = pi(memDisambig v,i)
	| getRegion _ = R.readonly

      (* pre-align allocptr *)
      val align = Alignment.build cluster
      fun alignAllocptr f = 
	if align f then 
           emit(M.MV(pty,allocptrR, M.ORB(pty,C.allocptr, M.LI 4)))
	else ()

      fun grabty(CPS.VAR v) = typmap v
	| grabty(CPS.LABEL v) = typmap v
	| grabty(CPS.INT _) = CPS.INTt
	| grabty(CPS.INT32 _) = CPS.INT32t
	| grabty(CPS.VOID) = CPS.FLTt
	| grabty _ = CPS.BOGt

      (* The baseptr contains the start address of the entire compilation unit *)
      fun laddr(lab, k) =
	M.ADD(pty,C.baseptr,
	      M.LABEL(LE.PLUS(LE.LABEL lab, 
		      LE.CONST(k-MachineSpec.constBaseRegOffset))))

      (* a CPS register may be implemented as a physical 
       * register or a memory location.
       *)
      fun assign(M.REG(ty,r), v) = M.MV(ty, r, v)
	| assign(r as M.LOAD(ty, ea, region), v) = M.STORE(ty, ea, v, region)
	| assign _ = error "assign"

      fun regbind(CPS.VAR v) = 
            ((Intmap.map gpRegTbl v) handle e => 
               (print ("\n* can't find a register for lvar " ^ (Int.toString v) ^ "\n");
                raise e))
	| regbind(CPS.INT i) = M.LI (i+i+1)
	| regbind(CPS.INT32 w) = M.LI32 w
	| regbind(CPS.LABEL v) = laddr(functionLabel v, 0)
	| regbind _ = error "regbind"

      fun fregbind(CPS.VAR v) = 
             ((Intmap.map fpRegTbl v) handle e =>
               (print ("\n* can't find a fpregister for lvar " ^ (Int.toString v) ^ "\n");
                raise e))
	| fregbind _ = error "fregbind"

      (* Add type bindings for each definition. This is used to determine
       * the parameter passing convention for standard functions.
       *)
      fun initTypBindings e = let
	val add = addTypBinding
      in
	case e
	of CPS.RECORD(_,_,v,e) => (add(v,CPS.BOGt); initTypBindings e)
	 | CPS.SELECT(_,_,v,t,e) => (add(v,t); initTypBindings e)
	 | CPS.OFFSET(_,_,v,e) => (add(v,CPS.BOGt); initTypBindings e)
	 | CPS.SWITCH(_,_,el) => app initTypBindings el
	 | CPS.SETTER(_,_,e) => initTypBindings e
	 | CPS.LOOKER(_,_,v,t,e) => (add(v,t); initTypBindings e)
	 | CPS.ARITH(_,_,v,t,e) => (add(v,t); initTypBindings e)
	 | CPS.PURE(_,_,v,t,e) => (add(v,t); initTypBindings e)
	 | CPS.BRANCH(_,_,_,e1,e2) => (initTypBindings e1; initTypBindings e2)
	 | CPS.APP _ => ()
	 | _ => error "initTypBindings"
      end

      (*   One entry to a function, the parameters will be in formal
       * parameter passing registers. Within the body of the function, they
       * are moved immediately to fresh temporary registers. This ensures
       * that the life time of the formal paramters is restricted to the 
       * function body and is critical in avoiding artificial register
       * interferences.
       *)
      fun initialRegBindingsEscaping(vl, rl, tl) = let
	fun eCopy(x::xs, M.GPR(M.REG(_,r))::rl, rds, rss, xs', rl') = let
	      val t = newReg()
	    in addRegBinding(x, t); eCopy(xs, rl, t::rds, r::rss, xs', rl')
	    end
	  | eCopy(x::xs, r::rl, rds, rss, xs', rl') = 
	      eCopy(xs, rl, rds, rss, x::xs', r::rl')
	  | eCopy([], [], [], [], xs', rl') = (xs', rl')
	  | eCopy([], [], rds, rss, xs', rl') = 
	     (emit(M.COPY(ity, rds, rss)); (xs', rl'))

	fun eOther(x::xs, M.GPR(r)::rl, xs', rl') = let
	      val t = newReg()
	    in addRegBinding(x, t); emit(M.MV(ity, t, r)); 
               eOther(xs, rl, xs', rl')
            end
	  | eOther(x::xs, (M.FPR(M.FREG(_,f)))::rl, xs', rl') = 
	      eOther(xs, rl, x::xs', f::rl')
	  | eOther([], [], xs, rl) = (xs, rl)

	fun eFcopy([], []) = ()
	  | eFcopy(xs, rl) = let
	      val fs = map (fn _ => newFreg()) xs
	    in
	      ListPair.app (fn (x,f) => addFregBinding(x,M.FREG(fty,f))) (xs,fs);
	      emit(M.FCOPY(fty, fs, rl))
	    end
	val (vl', rl') = eCopy(vl, rl, [], [], [], [])
      in
	eFcopy(eOther(vl', rl', [], []));
	ListPair.app addTypBinding (vl, tl)
      end

      fun initialRegBindingsKnown(vl, rl, tl) = let
	fun f(v, M.GPR(reg as M.REG _)) = addExpBinding(v, reg)
	  | f(v, M.FPR(freg as M.FREG _)) = addFregBinding(v, freg)
	  | f _ = error "initialRegBindingsKnown.f"
      in 
        ListPair.app f (vl, rl);
	ListPair.app addTypBinding (vl, tl)
      end

      fun updtHeapPtr(hp) = let
	fun advBy hp = emit(M.MV(pty, allocptrR, 
                                 M.ADD(pty, C.allocptr, M.LI hp)))
      in
	(* Keep allocation pointer aligned on odd boundary *)
	(* Note: We have accounted for the extra space this eats up in 
	 *    limit.sml
	 *)
	if hp = 0 then () 
        else if Word.andb(Word.fromInt hp, 0w4) <> 0w0 then advBy(hp+4)
	else advBy(hp)
      end

      fun testLimit hp = let
        fun assignCC(M.CC cc, v) = emit(M.CCMV(cc, v))
	  | assignCC _ = error "testLimit.assign"
      in
	updtHeapPtr(hp);
	case C.exhausted 
        of NONE => () 
         | SOME cc => assignCC(cc, gcTest)
	(*esac*)
      end

      (* Int 31 tag optimization *)
      val one  = M.LI 1
      val two  = M.LI 2

      fun addTag e = M.ADD(ity, e, one)
      fun stripTag e = M.SUB(ity, e, one)
      fun orTag e = M.ORB(ity, e, one)
      fun tag(signed, e) = let		(* true if signed *)
	fun double r = if signed then M.ADDT(ity,r,r) else M.ADD(ity, r,r)
      in
	case e 
	 of M.REG _ => addTag(double e)
          | _ => let
	       val tmp = newReg()
	     in M.SEQ(M.MV(ity, tmp, e), addTag(double (M.REG(ity,tmp))))
	     end
      end
      val mlZero = tag(false, M.LI 0)
      fun untag(_, CPS.INT i) = M.LI i
	| untag(true, v) = M.SRA(ity, regbind v, one)
	| untag(false, v) = M.SRL(ity, regbind v, one)

      fun int31add(addOp, [CPS.INT k, w]) = addOp(ity, M.LI(k+k), regbind w)
	| int31add(addOp, [w, v as CPS.INT _]) = int31add(addOp, [v,w])
	| int31add(addOp, [v,w]) = addOp(ity, regbind v, stripTag(regbind w))

      fun int31sub(subOp, [CPS.INT k,w]) = subOp(ity, M.LI(k+k+2), regbind w)
	| int31sub(subOp, [v, CPS.INT k]) = subOp(ity, regbind v, M.LI(k+k))
	| int31sub(subOp, [v,w]) = addTag(subOp(ity, regbind v, regbind w))

      fun int31xor([CPS.INT k, w]) = M.XORB(ity, M.LI(k+k), regbind w)
	| int31xor([w,v as CPS.INT _]) = int31xor [v,w]
	| int31xor([v,w]) = addTag (M.XORB(ity, regbind v, regbind w))

      fun int31mul(signed, args) = let
        val mulOp = if signed then M.MULT else M.MULU
	fun f [CPS.INT k, CPS.INT j] = addTag(mulOp(ity, M.LI(k+k), M.LI(j)))
	  | f [CPS.INT k, w] = addTag(mulOp(ity,untag(signed, w), M.LI(k+k)))
	  | f [v, w as CPS.INT _] = f ([w, v])
	  | f [v, w] = addTag(mulOp(ity,stripTag(regbind v), untag(signed, w)))
      in f args
      end

      fun int31div(signed, args) = let
	val divOp = if signed then M.DIVT else M.DIVU
	fun f [CPS.INT k, CPS.INT j] = divOp(ity,M.LI k, M.LI j)
	  | f [CPS.INT k, w] = divOp(ity,M.LI k, untag(signed, w))
	  | f [v, CPS.INT k] = divOp(ity,untag(signed, v), M.LI(k))
	  | f [v, w] = divOp(ity,untag(signed, v), untag(signed, w))
      in tag(signed, f args)
      end

      fun int31lshift [CPS.INT k, w] =
	    addTag (M.SLL(ity, M.LI(k+k), untag(false, w)))
	| int31lshift [v, CPS.INT k] = 
	    addTag(M.SLL(ity,stripTag(regbind v), M.LI(k)))
	| int31lshift [v,w] = 
	    addTag(M.SLL(ity,stripTag(regbind v), untag(false, w)))

      fun int31rshift(rshiftOp, [v, CPS.INT k]) =  
	    orTag(rshiftOp(ity, regbind v, M.LI(k)))
	| int31rshift(rshiftOp, [v,w]) =
	    orTag(rshiftOp(ity, regbind v, untag(false, w)))

      fun getObjDescriptor(v) = 
	M.LOAD(ity, M.SUB(pty, regbind v, M.LI(4)), getRegion(v, ~1))

      fun getObjLength(v) = 
	M.SRL(ity, getObjDescriptor(v), M.LI(D.tagWidth -1))

      (* Note: because formals are moved into fresh temporaries,
       * (formals intersection actuals) is empty. 
       *)
      fun callSetup(formals, actuals) = let
	fun gather([], [], cpRd, cpRs, fcopies, moves) = 
	     (case (cpRd,cpRs) of ([],[]) => () | _ => 
                  emit(M.COPY(ity, cpRd, cpRs));
	      case fcopies
                of [] => () 
	         | _ => emit(M.FCOPY(fty, map #1 fcopies, map #2 fcopies));
	      app emit moves)
	  | gather(M.GPR(M.REG(ty,rd))::fmls, act::acts, cpRd, cpRs, f, m) = 
	     (case regbind act
	       of M.REG(_,rs) => gather(fmls, acts, rd::cpRd, rs::cpRs, f, m)
   	        | e => gather(fmls, acts, cpRd, cpRs, f, M.MV(ty, rd, e)::m)
	     (*esac*))
	  | gather(M.GPR(M.LOAD(ty,ea,r))::fmls, act::acts, cpRd, cpRs, f, m) =
	     gather(fmls, acts, cpRd, cpRs, f, M.STORE(ty,ea,regbind act,r)::m)
	  | gather(M.FPR(M.FREG(ty,fd))::fmls, act::acts, cpRd, cpRs, f, m) = 
	     (case fregbind act
	       of M.FREG(_,fs) => gather(fmls, acts, cpRd, cpRs, (fd, fs)::f, m)
	        | e => gather(fmls, acts, cpRd, cpRs, f, M.FMV(ty, fd, e)::m)
	     (*esac*))
	  | gather _ = error "callSetup.gather"
      in
	gather(formals, actuals, [], [], [], [])
      end

      (* scale-and-add *)
      fun scale1(a, CPS.INT 0) = a
	| scale1(a, CPS.INT k) = M.ADD(ity, a, M.LI(k))
	| scale1(a, i) = M.ADD(ity, a, untag(true, i))

      fun scale4(a, CPS.INT 0) = a
	| scale4(a, CPS.INT i) = M.ADD(ity, a, M.LI(i*4))
	| scale4(a, i) = M.ADD(ity, a, M.SLL(ity, untag(true,i), two))
                                            

      fun scale8(a, CPS.INT 0) = a
	| scale8(a, CPS.INT i) = M.ADD(ity, a, M.LI(i*8))
	| scale8(a, i) = M.ADD(ity, a, M.SLL(ity, stripTag(regbind i), 
                                              M.LI(2)))

      (* add to storelist, the address where a boxed update has occured *)
      fun recordStore(tmp, hp) =
	(emit(M.STORE(pty, M.ADD(pty, C.allocptr, M.LI(hp)), tmp, R.storelist));
	 emit(M.STORE(pty, M.ADD(pty, C.allocptr, M.LI(hp+4)), C.storeptr, R.storelist));
	 emit(assign(C.storeptr, M.ADD(pty, C.allocptr, M.LI(hp)))))
	   
      fun unsignedCmp oper = case oper
	of P.>   => M.GTU | P.>=  => M.GEU | P.<   => M.LTU | P.<=  => M.LEU
 	 | P.eql => M.EQ  | P.neq => M.NE

      fun signedCmp oper = case oper
	of P.> => M.GT   | P.>= => M.GE   | P.< => M.LT    | P.<= => M.LE
 	 | P.neq => M.NE | P.eql => M.EQ 

      fun branchToLabel(lab) = M.JMP(M.LABEL(LE.LABEL(lab)), [lab])

      local
	open CPS
      in

      val offp0 = CPS.OFFp 0 

      fun alloc(x, e, rest, hp) = let
        val r = newReg()
      in
        addRegBinding(x, r);
	emit(M.MV(ity, r, e));  
	gen(rest, hp)
      end

      and falloc(x, e, rest, hp) = 
      (case treeify x
	 of CpsTreeify.DEAD => gen(rest, hp)
	  | CpsTreeify.TREEIFY => (addFregBinding(x,e); gen(rest, hp))
          | CpsTreeify.COMPUTE => let
	      val f = newFreg()
            in
	      addFregBinding(x, M.FREG(fty, f));  
	      emit(M.FMV(fty, f, e));  
	      gen(rest, hp)
            end
       (*esac*))

      and nop(x, v, e, hp) = alloc(x, regbind v, e, hp)

      and copy(x, v, rest, hp) = let
	val dst = newReg()
      in
	addRegBinding(x, dst);
	case regbind v
	 of M.REG(_,src) => emit(M.COPY(ity, [dst], [src]))
          | e => emit(M.MV(ity, dst, e))
	(*esac*);
        gen(rest, hp)
      end

      and branch (cmp, [v,w], d, e, hp) = let
	val trueLab = Label.newLabel""
      in
	(* is single assignment great or what! *)
	emit(M.BCC(cmp, M.CMP(32, cmp, regbind v, regbind w), trueLab));
	gen(e, hp);
	genlab(trueLab, d, hp)
      end

      and arith(oper, v, w, x, e, hp) = 
	alloc(x, oper(ity, regbind v, regbind w), e, hp)

      and orderedArith(oper, v, w, x, order, e, hp) = 
	alloc(x, oper(ity, regbind v, regbind w, order), e, hp)

      and logical(oper, v, w, x, e, hp) = 
	alloc(x, oper(ity, regbind v, untag(false, w)), e, hp)

      and genlab(lab, e, hp) = (comp (M.DEFINELABEL lab); gen(e, hp))

      and gen(RECORD((CPS.RK_SPILL | CPS.RK_CONT), vl, w, e), hp) =
	    gen(RECORD(CPS.RK_RECORD, vl, w, e), hp)
	| gen(RECORD(CPS.RK_FCONT, vl, w, e), hp) =
	    gen(RECORD(CPS.RK_FBLOCK, vl, w, e), hp)
	| gen(RECORD(CPS.RK_FBLOCK, vl, w, e), hp) = let
	    val len = List.length vl
	    val desc = dtoi(D.makeDesc(len+len, D.tag_raw64))
	    val vl' = 
	      map (fn (x, p as SELp _) => (M.GPR(regbind x), p)
		    | (x, p as OFFp 0) => (M.FPR(fregbind x), p)
		    | _ => error "gen:RECORD:RK_FBLOCK")
	          vl
	    val ptr = newReg()
	    (* At initialization the allocation pointer is aligned on
	     * an odd-word boundary, and the heap offset set to zero. If an
	     * odd number of words have been allocated then the heap pointer
	     * is misaligned for this record creation.
	     *)
	    val hp = 
	      if Word.andb(Word.fromInt hp, 0w4) <> 0w0 then hp+4 else hp
          in		  
	    addRegBinding(w, ptr);
	    MkRecord.frecord
	      {desc=M.LI desc, fields=vl', ans=ptr, mem=memDisambig w, 
               hp=hp, emit=emit};
	    gen(e, hp + 4 + len*8)
	  end
	| gen(RECORD(CPS.RK_VECTOR, vl, w, e), hp) = let
	    val len = length vl
	    val hdrDesc = dtoi(D.desc_polyvec)
	    val dataDesc = dtoi(D.makeDesc(len, D.tag_vec_data))
	    val contents = map (fn (v,p) => (regbind v, p)) vl
	    val dataPtr = newReg()
	    val hdrPtr = newReg()
	    val hdrM = memDisambig w
	    val dataM = hdrM (* Allen *)
	    in
	      addRegBinding(w, hdrPtr);
	      MkRecord.record {
		  desc = M.LI(dataDesc), fields = contents,
		  ans = dataPtr,
		  mem = dataM, hp = hp, emit=emit
		};
	      MkRecord.record {
		  desc = M.LI hdrDesc,
		  fields = [
		      (M.REG(ity,dataPtr), offp0),
		      (tag(false, M.LI len), offp0)
		    ],
		  ans = hdrPtr,
		  mem = hdrM, hp = hp + 4 + len*4, emit=emit
		};
	      gen (e, hp + 16 + len*4)
	    end
	| gen(RECORD(kind, vl, w, e), hp) = let
	    val len = length vl
	    val desc = case (kind, len)
	      of (CPS.RK_I32BLOCK, l) => dtoi(D.makeDesc (l, D.tag_raw32))
	       | (_, l) => dtoi(D.makeDesc (l, D.tag_record))
              (*esac*)
	    val contents = map (fn (v,p) => (regbind v, p)) vl
	    val ptr = newReg()
	  in
	    addRegBinding(w, ptr);
	    MkRecord.record 
	      {desc=M.LI desc, 
               fields=contents, ans=ptr, mem=memDisambig w, hp=hp, emit=emit};
	    gen(e, hp + 4 + len*4 )
          end

	(*** SELECT ***)
	| gen(SELECT(i,INT k,x,t,e), hp) =
           let val unboxedfloat = MS.unboxedFloats
               fun isFlt t = 
                 if unboxedfloat then (case t of FLTt => true | _ => false)
                 else false
               fun fallocSp(x,e,hp) =
                 (addFregBinding(x,M.FREG(fty,newFreg()));gen(e, hp))
              (* warning: the following generated code should never be 
                 executed; its semantics is completely screwed up !
               *)
            in if isFlt t then fallocSp(x, e, hp)
	       else alloc(x, M.LI k, e, hp)(* BOGUS *)
           end
	| gen(SELECT(i,v,x,FLTt,e), hp) = 
	    falloc(x, M.FLOAD(fty, scale8(regbind v, INT i), R.real), e, hp)
	| gen(SELECT(i,v,x,_,e), hp) = let
	    val select = M.LOAD(ity,scale4(regbind v, INT i), getRegion(v, i))
	  in
	    (* This business is only done with SELECTs because it is
	       where I think it has the most benefit. [Lal]
	     *)
	    case treeify x
	     of CpsTreeify.COMPUTE => alloc(x, select, e, hp)
	      | CpsTreeify.TREEIFY => (addExpBinding(x, select); gen(e, hp))
 	      | CpsTreeify.DEAD => gen(e, hp)
          end

	| gen(OFFSET(i,v,x,e), hp) = alloc(x, scale4(regbind v, INT i), e, hp)

	(*** APP ***)
	| gen(APP(INT k, args), hp) = updtHeapPtr(hp)
	| gen(APP(func as VAR f, args), hp) = let
	    val formals as (M.GPR dest::_) =  
	          ArgP.standard(typmap f, map grabty args)
	  in
	    callSetup(formals, args);
	    testLimit hp;
	    emit(M.JMP(dest, []));
	    comp(M.ESCAPEBLOCK(formals @ dedicated))
	  end
	| gen(APP(func as LABEL f, args), hp) = 
	  (case Intmap.map genTbl f
	    of Frag.KNOWNFUN(ref(Frag.GEN formals)) => 
	        (updtHeapPtr(hp);
		 callSetup(formals, args); 
		 emit(branchToLabel(functionLabel f)))
	     | Frag.KNOWNFUN(r as ref(Frag.UNGEN(f,vl,tl,e))) => let
		 val formals = ArgP.known tl
		 val lab = functionLabel f
	       in
		 r := Frag.GEN formals;
		 updtHeapPtr(hp);
		 callSetup(formals, args);
		 comp(M.DEFINELABEL lab);
		 comp(M.BLOCK_NAME(Int.toString f));
		 alignAllocptr f;
 		 initialRegBindingsEscaping(vl, formals, tl);
		 initTypBindings e;
		 gen(e, 0)
	       end
	     | Frag.KNOWNCHK(r as ref(Frag.UNGEN(f,vl,tl,e))) => let
	         val formals = 
		   if MS.fixedArgPassing then ArgP.fixed tl
		   else ArgP.known tl
		 val lab = functionLabel f
	       in
		 r:=Frag.GEN formals;
		 callSetup(formals, args);
		 testLimit hp;
(*** CAN WE REMOVE THIS BRANCH??? ***)
		 emit(branchToLabel(lab));
		 comp(M.DEFINELABEL lab);
		 comp(M.BLOCK_NAME(Int.toString f));
		 CallGc.knwCheckLimit {emit=emit,comp=comp}
		   {maxAlloc=4*maxAlloc f, regfmls=formals, regtys=tl, 
		    return=branchToLabel(lab)};
	         alignAllocptr f;
		 initialRegBindingsEscaping(vl, formals, tl);
		 initTypBindings e;
		 gen(e, 0)
	       end
	     | Frag.KNOWNCHK(ref(Frag.GEN formals)) => 
	         (callSetup(formals, args); 
		  testLimit hp;
		  emit(branchToLabel(functionLabel f)))
	     | Frag.STANDARD{fmlTyps, ...} => let
	         val formals = ArgP.standard(typmap f, fmlTyps)
	       in
		 callSetup(formals, args);
		 testLimit hp;
		 emit(branchToLabel(functionLabel f))
	       end
	  (*esac*))
	(*** SWITCH ***)
	| gen(SWITCH(INT _, _, _), hp) = error "SWITCH"
	| gen(SWITCH(v, _, l), hp) = let
	    val lab = Label.newLabel""
	    val labs = map (fn _ => Label.newLabel"") l
	    val tmpR = newReg() val tmp = M.REG(ity,tmpR)
	  in
	    emit(M.MV(ity, tmpR, laddr(lab, 0)));
	    emit(M.JMP(M.ADD(pty, tmp, M.LOAD(pty, scale4(tmp, v), R.readonly)), labs));
	    comp(M.PSEUDO_OP(PseudoOp.JUMPTABLE{base=lab, targets=labs}));
	    ListPair.app (fn (lab, e) => genlab(lab, e, hp)) (labs, l)
	  end

	(*** PURE ***)
	| gen(PURE(P.pure_arith{oper=P.orb, ...}, [v,w], x, _, e), hp) = 
	    alloc(x, M.ORB(ity, regbind v, regbind w), e, hp)
	| gen(PURE(P.pure_arith{oper=P.andb, ...}, [v,w], x, _, e), hp) = 
	    alloc(x, M.ANDB(ity, regbind v, regbind w), e, hp)
	| gen(PURE(P.pure_arith{oper, kind}, args as [v,w], x, ty, e), hp) = 
	  (case kind
	    of P.INT 31 => (case oper
	         of P.xorb   => alloc(x, int31xor(args), e, hp)
	          | P.lshift => alloc(x, int31lshift args, e, hp)
		  | P.rshift => alloc(x, int31rshift(M.SRA,args), e, hp)
		  | _ => error "gen:PURE INT 31"
	        (*esac*))	
	     | P.INT 32  => (case oper
	         of P.xorb  => arith(M.XORB, v, w, x, e, hp)
	          | P.lshift => logical(M.SLL, v, w, x, e, hp)
		  | P.rshift => logical(M.SRA, v, w, x, e, hp)
		  | _ => error "gen:PURE INT 32"
	        (*esac*))
	     | P.UINT 31 => (case oper
		 of P.+    => alloc(x, int31add(M.ADD, args), e, hp)
	          | P.-    => alloc(x, int31sub(M.SUB, args), e, hp)
		  | P.*    => alloc(x, int31mul(false, args), e, hp)
		  | P./    => (* This is not really a pure 
			         operation -- oh well *)
			       (updtHeapPtr hp;
				alloc(x, int31div(false, args), e, 0))
	          | P.xorb => alloc(x, int31xor(args), e, hp)
	          | P.lshift  => alloc(x, int31lshift args, e, hp)
		  | P.rshift  => alloc(x, int31rshift(M.SRA,args), e, hp)
		  | P.rshiftl => alloc(x, int31rshift(M.SRL,args), e, hp)
		  | _ => error "gen:PURE UINT 31"
	        (*esac*))
	     | P.UINT 32 => (case oper
		 of P.+     => arith(M.ADD, v, w, x, e, hp)
 	          | P.-     => arith(M.SUB, v, w, x, e, hp)
		  | P.*     => arith(M.MULU, v, w, x, e, hp)
		  | P./     => (updtHeapPtr hp; 
				arith(M.DIVU, v, w, x, e, 0))
	          | P.xorb  => arith(M.XORB, v, w, x, e, hp)
	          | P.lshift => logical(M.SLL, v, w, x, e, hp)
		  | P.rshift => logical(M.SRA, v, w, x, e, hp)
		  | P.rshiftl=> logical(M.SRL, v, w, x, e, hp)
		  | _ => error "gen:PURE UINT 32"
	        (*esac*))
	  (*esac*))
	| gen(PURE(P.pure_arith{oper=P.notb, kind}, [v], x, _, e), hp) =
	  (case kind 
	    of P.UINT 32 => alloc(x, M.XORB(ity, regbind v, M.LI32 0wxFFFFFFFF), e, hp)
   	     | P.INT 32 => alloc(x, M.XORB(ity, regbind v, M.LI32 0wxFFFFFFFF), e, hp)
	     | P.UINT 31 => alloc(x, M.SUB(ity, M.LI 0, regbind v), e, hp)
	     | P.INT 31 => alloc(x, M.SUB(ity, M.LI 0, regbind v), e, hp)
	  (*esac*))
	| gen(PURE(P.copy ft, [v], x, _, e), hp) =
	   (case ft
	    of (31, 32) => alloc(x, M.SRL(ity, regbind v, one), e, hp)
	     | (8, 31) => copy(x, v, e, hp)
	     | (8, 32) => alloc(x, M.SRL(ity, regbind v, one), e, hp)
	     | (n,m) => if n = m then copy(x, v, e, hp) else error "gen:PURE:copy"
           (*esac*))
	| gen(PURE(P.extend ft, [v], x, _ ,e), hp) = 
	  (case ft
	   of (8,31) => 
	        alloc(x, M.SRA(ity, M.SLL(ity, regbind v,M.LI 23), M.LI 23),
                      e, hp)
	    | (8,32) =>
		alloc(x, M.SRA(ity, M.SLL(ity, regbind v, M.LI 23), M.LI 24), 
		      e, hp)
	    | (31,32) => alloc(x, M.SRA(ity, regbind v, one), e, hp)
            | (n, m) => 
		if n = m then copy(x, v, e, hp) else error "gen:PURE:extend"
            (*esac*))
	| gen(PURE(P.trunc ft, [v], x, _, e), hp) = 
	  (case ft
	   of (32, 31) => 
	        alloc(x, M.ORB(ity, M.SLL(ity, regbind v, one), one), e, hp)
	    | (31, 8) => alloc(x, M.ANDB(ity, regbind v, M.LI 0x1ff), e, hp)
	    | (32, 8) => alloc(x, tag(false, M.ANDB(ity, regbind v, M.LI 0xff)), e, hp)
	    | (n, m) => if n = m then copy(x, v, e, hp) else error "gen:PURE:trunc"
	   (*esac*))
	| gen(PURE(P.real{fromkind=P.INT 31, tokind}, [v], x, _, e), hp) = 
	  (case tokind
	    of P.FLOAT 64 => (case v
		 of INT n => falloc(x, M.CVTI2F(fty,M.SIGN_EXTEND,M.LI n), e, hp)
	          | _ => falloc(x, M.CVTI2F(fty,M.SIGN_EXTEND,untag(true, v)), e, hp)
	        (*esac*))
	     | _ => error "gen:PURE:P.real"
          (*esac*))
	| gen(PURE(P.pure_arith{oper, kind=P.FLOAT 64}, [v], x, _, e), hp) = let
	    val r = fregbind v
	  in
	    case oper
	     of P.~ => falloc(x, M.FNEG(fty,r), e, hp)
	      | P.abs => falloc(x, M.FABS(fty,r), e, hp)
          end
	| gen(PURE(P.objlength, [v], x, _, e), hp) = 
	    alloc(x, orTag(getObjLength(v)), e, hp)
	| gen(PURE(P.length, [v], x, t, e), hp) =
	  (* array/vector length operation *)
	    gen(SELECT(1, v, x, t, e), hp)
	| gen(PURE(P.subscriptv, [v, INT i], x, t, e), hp) = let
	    val region = getRegion(v, 0)
	    val a = M.LOAD(ity, regbind v, region)  (* get data pointer *)
	    val region' = region (* Allen *)
	    in
	      alloc(x, M.LOAD(ity, scale4(a, INT i), region'), e, hp)
	    end
	| gen(PURE(P.subscriptv, [v, w], x, _, e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      alloc (x, M.LOAD(ity, scale4(a, w), R.readonly), e, hp)
	    end
	| gen(PURE(P.pure_numsubscript{kind=P.INT 8}, [v,i], x, _, e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      alloc(x, tag(false,M.LOAD(8,scale1(a, i), R.memory)), e, hp) 
	    end
	| gen(PURE(P.gettag, [v], x, _, e), hp) = 
	    alloc(x, 
		  tag(false, M.ANDB(ity,
                        getObjDescriptor(v), M.LI(D.powTagWidth-1))),
		  e, hp)
	| gen(PURE(P.mkspecial, [i, v], x, _, e), hp) = let
	    val desc = case i
	      of INT n => M.LI(dtoi(D.makeDesc(n, D.tag_special)))
	       | _ => M.ORB(ity, M.SLL(ity, untag(true, i), M.LI D.tagWidth),
			   M.LI(dtoi D.desc_special))
	    val ptr = newReg()
	  in
	    MkRecord.record{desc=desc, fields=[(regbind v, offp0)],
			    ans=ptr, mem=memDisambig x, hp=hp, emit=emit};
	    addRegBinding(x, ptr);
	    gen(e, hp+8)
	  end
	| gen(PURE(P.makeref, [v], x, _, e), hp) = let
	    val ptr = newReg()
	    val tag = M.LI(dtoi D.desc_ref)
	    val mem = memDisambig x
	  in
	    emit(M.STORE(ity, M.ADD(pty, C.allocptr, M.LI hp), tag, mem));
	    emit(M.STORE(ity, M.ADD(pty, C.allocptr, M.LI(hp+4)), regbind v, mem));
	    emit(M.MV(pty, ptr, M.ADD(pty, C.allocptr, M.LI(hp+4))));
	    addRegBinding(x, ptr);
	    gen(e, hp+8)
	  end
	| gen(PURE(P.fwrap,[u],w,_,e), hp) = 
	    gen(RECORD(CPS.RK_FBLOCK,[(u, offp0)],w,e), hp)
	| gen(PURE(P.funwrap,[u],w,_,e), hp) = gen(SELECT(0,u,w,FLTt,e), hp)
	| gen(PURE(P.iwrap,[u],w,_,e), _) = error "iwrap not implemented"
	| gen(PURE(P.iunwrap,[u],w,_,e), _) = error "iunwrap not implemented"
	| gen(PURE(P.i32wrap,[u],w,_,e), hp) = 
	    gen(RECORD(CPS.RK_I32BLOCK,[(u, offp0)],w,e), hp)
	| gen(PURE(P.i32unwrap,[u],w,_,e), hp) = gen(SELECT(0,u,w,INT32t,e), hp)
	| gen(PURE(P.wrap,[u],w,_,e), hp) = copy(w, u, e, hp)
	| gen(PURE(P.unwrap,[u],w,_,e), hp) = copy(w, u, e, hp)
	| gen(PURE(P.cast,[u],w,_,e), hp) = copy(w, u, e, hp)
	| gen(PURE(P.getcon,[u],w,t,e), hp) = gen(SELECT(0,u,w,t,e), hp)
	| gen(PURE(P.getexn,[u],w,t,e), hp) = gen(SELECT(0,u,w,t,e), hp)
	| gen(PURE(P.getseqdata, [u], x, t, e), hp) = gen(SELECT(0,u,x,t,e), hp)
	| gen(PURE(P.recsubscript, [v, INT w], x, t, e), hp) = 
	    gen(SELECT(w, v, x, t, e), hp)
	| gen(PURE(P.recsubscript, [v, w], x, _, e), hp) =
	    alloc(x, M.LOAD(ity, scale4(regbind v, w), R.readonly), e, hp)
	| gen(PURE(P.raw64subscript, [v, INT i], x, _, e), hp) =
	    gen(SELECT(i, v, x, FLTt, e), hp)
	| gen(PURE(P.raw64subscript, [v, i], x, _, e), hp) =
	    falloc(x, M.FLOAD(fty,scale8(regbind v, i),R.readonly), e, hp)
	| gen(PURE(P.newarray0, [_], x, t, e), hp) = let
	    val hdrDesc = dtoi(D.desc_polyarr)
	    val dataDesc = dtoi D.desc_ref
	    val dataPtr = newReg()
	    val hdrPtr = newReg()
	    val hdrM = memDisambig x
	    val (tagM, valM) = (hdrM, hdrM) (* Allen *)
	    in
	      addRegBinding(x, hdrPtr);
	    (* gen code to allocate "ref()" for array data *)
	      emit(M.STORE(ity, M.ADD(pty, C.allocptr, M.LI hp), M.LI dataDesc, tagM));
	      emit(M.STORE(ity, M.ADD(pty, C.allocptr, M.LI(hp+4)), mlZero, valM));
	      emit(M.MV(pty, dataPtr, M.ADD(pty, C.allocptr, M.LI(hp+4))));
	    (* gen code to allocate array header *)
	      MkRecord.record {
		  desc = M.LI hdrDesc,
		  fields = [(M.REG(ity,dataPtr), offp0), (mlZero, offp0)],
		  ans = hdrPtr,
		  mem = hdrM, hp = hp + 8, emit=emit
		};
	      gen (e, hp + 20)
	    end
	(*** ARITH ***)
	| gen(ARITH(P.arith{kind=P.INT 31, oper}, args, x, _, e), hp) = 
	  (updtHeapPtr hp;
	   case oper
	    of P.+ => alloc(x, int31add(M.ADDT, args), e, 0)
	     | P.- => alloc(x, int31sub(M.SUBT, args), e, 0)
	     | P.* => alloc(x, int31mul(true, args), e, 0)
	     | P./ => alloc(x, int31div(true, args), e, 0)
	     | P.~ => alloc(x, M.SUBT(ity, M.LI 2, regbind(hd args)), e, 0)
	     | _ => error "gen:ARITH INT 31"
	  (*esac*))	
	| gen(ARITH(P.arith{kind=P.INT 32, oper}, [v,w], x, _, e), hp) =
	  (updtHeapPtr hp;
	   case oper
	    of P.+     => arith(M.ADDT, v, w, x, e, 0)
	     | P.-     => arith(M.SUBT, v, w, x, e, 0)
	     | P.*     => arith(M.MULT, v, w, x, e, 0)
	     | P./     => arith(M.DIVT, v, w, x, e, 0)
	     | _ => error "P.arith{kind=INT 32, oper}, [v,w], ..."
	  (*esac*))
	| gen(ARITH(P.arith{kind=P.INT 32, oper=P.~ }, [v], x, _, e), hp) =
	    (updtHeapPtr hp;
	     alloc(x, M.SUBT(ity, M.LI 0, regbind v), e, 0))

	  (* Note: for testu operations we use a somewhat arcane method
	   * to generate traps on overflow conditions. A better approach
	   * would be to generate a trap-if-negative instruction available
	   * on a variety of machines, e.g. mips and sparc (maybe others).
	   *)
	| gen(ARITH(P.testu(32, 32), [v], x, _, e), hp) = let
	     val xreg = newReg()
	     val vreg = regbind v
	  in
	    updtHeapPtr hp;
	    emit(M.MV(ity,xreg,M.ADDT(ity, vreg, regbind(INT32 0wx80000000))));
	    alloc(x, vreg, e, 0)
          end
	| gen(ARITH(P.testu(31, 31), [v], x, _, e), hp) = let
	     val xreg = newReg()
	     val vreg = regbind v
	  in
	    updtHeapPtr hp;
	    emit(M.MV(ity,xreg,M.ADDT(ity, vreg, regbind(INT32 0wx80000000))));
	    alloc(x, vreg, e, 0)
          end
	| gen(ARITH(P.testu(32,31), [v], x, _, e), hp) = let
	    val vreg = regbind v
	    val tmp = newReg()
	    val tmpR = M.REG(ity,tmp)
	    val lab = Label.newLabel ""
	  in
	    emit(M.MV(ity, tmp, regbind(INT32 0wx3fffffff)));
	    emit(M.BCC(M.LEU, M.CMP(32, M.LEU, vreg, tmpR), lab));
	    updtHeapPtr hp;
	    emit(M.MV(ity, tmp, M.SLL(ity, tmpR, one)));
	    emit(M.MV(ity, tmp, M.ADDT(ity, tmpR, tmpR)));
	    comp(M.DEFINELABEL lab);
	    alloc(x, tag(false, vreg), e, hp)
          end
	| gen(ARITH(P.test(32,31), [v], x, _, e), hp) = 
	   (updtHeapPtr hp; alloc(x, tag(true, regbind v), e, 0))
	| gen(ARITH(P.test(n, m), [v], x, _, e), hp) = 
	   if n = m then copy(x, v, e, hp) else error "gen:ARITH:test"
	| gen(ARITH(P.arith{oper, kind=P.FLOAT 64}, vl, x, _, e), hp) = let
	    fun binary(oper, [v,w]) = 
	      falloc(x, oper(fty, fregbind v, fregbind w), e, hp)
	  in
	    case oper
	     of P.+ => binary(M.FADD, vl)
	      | P.* => binary(M.FMUL, vl)
	      | P.- => binary(M.FSUB, vl)
	      | P./ => binary(M.FDIV, vl)
	  end
	(*** LOOKER ***)
	| gen(LOOKER(P.!, [v], x, _, e), hp) = 
	    alloc (x, M.LOAD(ity, regbind v, R.memory), e, hp)
	| gen(LOOKER(P.subscript, [v,w], x, _, e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      alloc (x, M.LOAD(ity, scale4(a, w), R.memory), e, hp)
	    end
	| gen(LOOKER(P.numsubscript{kind=P.INT 8},[v,i],x,_,e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      alloc(x, tag(false, M.LOAD(8,scale1(a, i),R.memory)), e, hp)
	    end
	| gen(LOOKER(P.numsubscript{kind=P.FLOAT 64}, [v,i], x, _, e), hp) = let
	    val a = M.LOAD(ity,regbind v, R.readonly)  (* get data pointer *)
	    in
	      falloc(x, M.FLOAD(fty,scale8(a, i),R.memory), e, hp)
	    end
	| gen(LOOKER(P.gethdlr,[],x,_,e), hp) = alloc(x, C.exnptr, e, hp)
	| gen(LOOKER(P.getvar, [], x, _, e), hp) = alloc(x, C.varptr, e, hp)
	| gen(LOOKER(P.deflvar, [], x, _, e), hp) = alloc(x, M.LI 0, e, hp)
	| gen(LOOKER(P.getspecial, [v], x, _, e), hp) = 
	    alloc(x, 
		  orTag(M.SRA(ity, getObjDescriptor(v),
			           M.LI (D.tagWidth-1))), 
		  e, hp)
	| gen(LOOKER(P.getpseudo, [i], x, _, e), hp) = 
	    (print "getpseudo not implemented\n"; nop(x, i, e, hp))
	(*** SETTER ***)
	| gen(SETTER(P.assign, [a as VAR arr, v], e), hp) = let
	    val ea = regbind a
	    in
	      recordStore(ea, hp);
	      emit(M.STORE(ity, ea, regbind v, memDisambig arr));
	      gen(e, hp+8)
	    end
	| gen(SETTER(P.unboxedassign, [a, v], e), hp) = 
	   (emit(M.STORE(ity, regbind a, regbind v, R.memory));
	    gen(e, hp))
	| gen(SETTER(P.update, [v,i,w], e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    val tmpR = newReg() 
	    val tmp = M.REG(ity, tmpR)
	    val ea = scale4(a, i)  (* address of updated cell *)
	    in
	      emit(M.MV(ity, tmpR, ea));
	      recordStore(tmp, hp);
	      emit(M.STORE(ity, tmp, regbind w, R.memory));
	      gen(e, hp+8)
	    end
	| gen(SETTER(P.boxedupdate, args, e), hp) = 
	    gen(SETTER(P.update, args, e), hp)
	| gen(SETTER(P.unboxedupdate, [v, i, w], e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      emit(M.STORE(ity, scale4(a, i), regbind w, R.memory));
	      gen(e, hp)
	    end
	| gen(SETTER(P.numupdate{kind=P.INT 8}, [s,i,v], e), hp) = let
	    val a = M.LOAD(ity, regbind s, R.readonly)  (* get data pointer *)
	    val ea = scale1(a, i)
	    in
	      emit(M.STORE(8,ea, untag(false, v), R.memory));
	      gen(e, hp)
	    end
	| gen(SETTER(P.numupdate{kind=P.FLOAT 64},[v,i,w],e), hp) = let
	    val a = M.LOAD(ity, regbind v, R.readonly)  (* get data pointer *)
	    in
	      emit(M.FSTORE(fty,scale8(a, i), fregbind w, R.memory)); 
	      gen(e, hp)
	    end
	| gen(SETTER(P.setspecial, [v, i], e), hp) = let
	    val ea = M.SUB(ity, regbind v, M.LI 4)
	    val i' = case i
	      of INT k => M.LI(dtoi(D.makeDesc(k, D.tag_special)))
	       | _ => M.ORB(ity, M.SLL(ity, untag(true, i), M.LI D.tagWidth),
			         M.LI(dtoi D.desc_special))
	  in
	    emit(M.STORE(ity, ea, i',R.memory));
	    gen(e, hp)
	  end
	| gen(SETTER(P.sethdlr,[x],e), hp) = 
	    (emit(assign(C.exnptr, regbind x));  gen(e, hp))
	| gen(SETTER(P.setvar,[x],e), hp) = 
	    (emit(assign(C.varptr, regbind x));  gen(e, hp))
	| gen(SETTER(P.uselvar,[x],e), hp) = gen(e, hp)
	| gen(SETTER(P.acclink,_,e), hp) = gen(e, hp)
	| gen(SETTER(P.setmark,_,e), hp) = gen(e, hp)
	| gen(SETTER(P.free,[x],e), hp) = gen(e, hp)
	| gen(SETTER(P.setpseudo,_,e), hp) = 
	    (print "setpseudo not implemented\n"; gen(e, hp))

	(*** BRANCH  ***)
        | gen(BRANCH(P.cmp{oper,kind=P.INT 31},[INT v, INT k],_,e,d), hp) = let
	    val itow = Word.fromInt
	  in
	    if (case oper 
		  of P.> => v>k | P.>= => v>=k | P.< => v<k | P.<= => v<=k
		   | P.eql => v=k | P.neq => v<>k
		(*esac*)) then  gen(e, hp)
	    else gen(d, hp)
	  end
        | gen(BRANCH(P.cmp{oper, kind=P.INT 31}, vw, _, e, d), hp) = 
	    branch(signedCmp oper, vw, e, d, hp)
        | gen(BRANCH(P.cmp{oper,kind=P.UINT 31},[INT v', INT k'],_,e,d), hp) = let
	    open Word
	    val v = fromInt v' 
	    val k = fromInt k'
          in
	    if (case oper
		 of P.> => v>k   | P.>= => v>=k  | P.< => v<k   | P.<= => v<=k
	          | P.eql => v=k | P.neq => v<>k
		(*esac*)) then
	      gen(e, hp)
	    else gen(d, hp)
	  end
        | gen(BRANCH(P.cmp{oper, kind=P.UINT 31}, vw, _, e, d), hp) = 
	    branch(unsignedCmp oper, vw, e, d, hp)
        | gen(BRANCH(P.cmp{oper,kind=P.UINT 32},[INT32 v,INT32 k],_,e,d), hp) = let
	    open Word32
          in
	    if (case oper
		 of P.> => v>k   | P.>= => v>=k  | P.< => v<k   | P.<= => v<=k
	          | P.eql => v=k | P.neq => v<>k
		(*esac*)) then
	      gen(e, hp)
	    else gen(d, hp)
	  end
        | gen(BRANCH(P.cmp{oper, kind=P.UINT 32}, vw, _, e, d), hp) = 
	    branch(unsignedCmp oper, vw, e, d, hp)

        | gen(BRANCH(P.cmp{oper, kind=P.INT 32}, vw, _, e, d), hp) = 
	    branch(signedCmp oper, vw, e, d, hp)
        | gen(BRANCH(P.fcmp{oper,size=64}, [v,w], _, d, e), hp) = let
	    val trueLab = Label.newLabel""
	    val fcond = case oper
	      of P.fEQ => M.==  | P.fULG => M.?<> 
	       | P.fUN => M.?   | P.fLEG => M.<=> 
	       | P.fGT => M.>   | P.fGE  => M.>=  
	       | P.fUGT => M.?> | P.fUGE => M.?>= 
	       | P.fLT => M.<   | P.fLE  => M.<=  
	       | P.fULT => M.?< | P.fULE => M.?<= 
	       | P.fLG => M.<>  | P.fUE  => M.?= 

	    val cmp = M.FCMP(64, fcond, fregbind v, fregbind w) 
          in
	    emit(M.FBCC(fcond, cmp, trueLab)); 
	    gen(e, hp);
	    genlab(trueLab, d, hp)
          end
	| gen(BRANCH(P.peql, vw, _,e,d), hp) = branch(M.EQ, vw, e, d, hp)
        | gen(BRANCH(P.pneq, vw, _, e, d), hp) = branch(M.NE, vw, e, d, hp)
        | gen(BRANCH(P.strneq, [n,v,w],c,d,e), hp) = 
	    gen(BRANCH(P.streq, [n,v,w],c,e,d), hp)
	| gen(BRANCH(P.streq, [INT n,v,w],_,d,e), hp) = let
	    val n' = ((n+3) div 4) * 4
	    val false_lab = Label.newLabel ""
	    val r1 = newReg()
	    val r2 = newReg()
	    fun cmpWord(i) = 
	      M.CMP(32, M.NE, M.LOAD(ity,M.ADD(ity,M.REG(ity, r1),i),R.readonly), 
		          M.LOAD(ity,M.ADD(ity,M.REG(ity, r2),i),R.readonly))
	    fun unroll i = 
	      if i=n' then ()
	      else (emit(M.BCC(M.NE, cmpWord(M.LI(i)), false_lab));
		    unroll (i+4))
          in
	      emit(M.MV(ity, r1, M.LOAD(ity, regbind v, R.readonly)));
	      emit(M.MV(ity, r2, M.LOAD(ity, regbind w, R.readonly)));
	      unroll 0;
              gen(d, hp);
	      genlab(false_lab, e, hp)
	  end
	| gen(BRANCH(P.boxed, [x], _, a, b), hp) = let
	    val lab = Label.newLabel""
	    val cmp = M.CMP(32, M.NE, M.ANDB(ity, regbind x, one), M.LI 0)
          in
	    emit(M.BCC(M.NE, cmp, lab));
	    gen(a, hp);
	    genlab(lab, b, hp)
	  end
        | gen(BRANCH(P.unboxed, x,c,a,b), hp) = gen(BRANCH(P.boxed,x,c,b,a), hp)
	| gen(e, hp) =  (PPCps.prcps e; print "\n"; error "genCluster.gen")
      end (*local*)

      fun fragComp () = let
	fun continue () = fcomp (Frag.next())
	and fcomp(NONE) = ()
	  | fcomp(SOME(_, Frag.KNOWNFUN _)) = continue ()
	  | fcomp(SOME(_, Frag.KNOWNCHK _)) = continue ()
	  | fcomp(SOME(_, Frag.STANDARD{func=ref NONE, ...})) = continue ()
	  | fcomp(SOME(lab, Frag.STANDARD{func as ref(SOME (zz as (_,f,vl,tl,e))), 
					  ...})) = let
	      val regfmls as (M.GPR linkreg::_) = ArgP.standard(typmap f, tl)
	      val baseval = 
		M.ADD(pty,linkreg, 
		      M.LABEL(LE.MINUS(LE.CONST MachineSpec.constBaseRegOffset,
				      LE.LABEL lab)))
	    in
	      func := NONE;
	      comp(M.PSEUDO_OP PseudoOp.ALIGN4);
	      comp(M.ENTRYLABEL lab);
	      comp(M.BLOCK_NAME(Int.toString f));
	      alignAllocptr f;
	      emit(assign(C.baseptr, baseval));
	      CallGc.stdCheckLimit {emit=emit,comp=comp}
                  {maxAlloc=4 * maxAlloc f, regfmls=regfmls, 
		   regtys=tl, return=M.JMP(linkreg,[])};
	      clearTables();
	      initialRegBindingsEscaping(vl, regfmls, tl);
	      initTypBindings e;
              if !Control.CG.printit then (
              print "************************************************* \n";
              PPCps.printcps0 zz;
              print "************************************************* \n")
              else ();
	      continue(gen(e, 0))
	    end
      in
	fcomp (Frag.next())
      end (* fragComp *)

      (* execution starts at the first CPS function -- the frag 
       * is maintained as a queue.
       *)
      fun initFrags (start::rest : CPS.function list) = let
	fun init(func as (fk, f, _, _, _)) = 
	  Intmap.add genTbl (f, Frag.makeFrag(func, functionLabel f))
      in
	app init rest;
	init start
      end
    in
      initFrags cluster;
      comp(M.BEGINCLUSTER);
      fragComp();
      CallGc.emitLongJumpsToGCInvocation {emit=emit,comp=comp} (regmap);
      comp(M.ENDCLUSTER(regmap,[]))
    end (* genCluster *)

    and emitMLRiscUnit(f) = let 
      val _ = Cells.reset()
      val regmap = Cells.regmap()
    in
      comp (M.BEGINCLUSTER);
      f regmap; 
      comp (M.ENDCLUSTER(regmap,[]))
    end
  in
    app mkGlobalTables funcs;
    app genCluster (Cluster.cluster funcs);
    emitMLRiscUnit (CallGc.emitModuleGC {emit=emit,comp=comp})
  end (* codegen *)
end (* MLRiscGen *)

(*
 * $Log: mlriscGen.sml,v $
 * Revision 1.12  1999/01/18 15:49:29  george
 *   support of interactive loading of MLRISC optimizer
 *
 * Revision 1.11  1998/11/23 20:09:42  jhr
 *   Fixed length field of raw64 objects (should be in words); new raw64Subscript
 *   primop.
 *
 * Revision 1.10  1998/11/18 03:53:11  jhr
 *  New array representations.
 *
 * Revision 1.9  1998/10/28 18:20:49  jhr
 *   Removed code generator support for STRING/REAL constants.
 *
 * Revision 1.8  1998/10/19 13:28:18  george
 *   Known functions once again move their arguments to fresh temporaries.
 *   The problem has to do with spilling and adjusting the regmask.
 *
 * Revision 1.7  1998/10/15 17:56:54  george
 *   known functions do not move formals to fresh temps
 *
 * Revision 1.6  1998/09/30 18:56:35  dbm
 * removed sharing/defspec conflict, using where structure
 *
 * Revision 1.5  1998/08/28 12:58:27  george
 *   Fix for bug1422: Core dump on Sparc when using lazy features
 *
 * Revision 1.4  1998/07/25 03:05:36  george
 *   changes to support block names in MLRISC
 *
 * Revision 1.3  1998/05/23 14:09:26  george
 *   Fixed RCS keyword syntax
 *
 *)

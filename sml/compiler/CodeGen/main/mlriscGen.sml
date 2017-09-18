(* mlriscGen.sml --- translate CPS to MLRISC.
 *
 * This version of MLRiscGen also injects GC types to the MLRISC backend.
 * I've also reorganized it a bit and added a few comments
 * so that I can understand it.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature MLRISCGEN =
sig
  val codegen : { funcs: CPS.function list,
		  limits:  CPS.lvar -> int * int,
		  err: ErrorMsg.complainer,
		  source: string }
		-> (unit -> int)
    (* The result is a thunk around the address of the resulting code
     * object's entry point.  The client must promise to first call
     * "finish" before forcing it. *)
end



functor MLRiscGen
   (structure MachineSpec: MACH_SPEC
    structure Ext        : SMLNJ_MLTREE_EXT
    structure C          : CPSREGS
		 	   where T.Region = CPSRegions
	                     and T.Constant = SMLNJConstant
		  	     and T.Extension = Ext
    structure ClientPseudoOps : SMLNJ_PSEUDO_OPS
    structure PseudoOp   : PSEUDO_OPS
			    where T = C.T
			      and Client = ClientPseudoOps
    structure MLTreeComp : MLTREECOMP
			   where TS.T = C.T
                             and TS.S.P = PseudoOp
    structure Flowgen    : CONTROL_FLOWGRAPH_GEN
			   where S = MLTreeComp.TS.S
			     and I = MLTreeComp.I
			     and CFG = MLTreeComp.CFG
    structure InvokeGC   : INVOKE_GC
			   where TS = MLTreeComp.TS
			     and CFG = Flowgen.CFG

    structure Cells      : CELLS
    structure CCalls     : C_CALLS
			   where T = C.T
    val compile : Flowgen.CFG.cfg -> unit
 ) : MLRISCGEN =
struct

  structure M  = C.T			(* MLTree *)
  structure E  = Ext			(* Extensions *)
  structure P  = CPS.P			(* CPS primitive operators *)
  structure R  = CPSRegions		(* Regions *)
  structure PT = R.PT			(* PointsTo *)
  structure CG = Control.CG		(* Compiler Control *)
  structure MS = MachineSpec		(* Machine Specification *)
  structure D  = MS.ObjDesc		(* ML Object Descriptors *)
  structure TS = MLTreeComp.TS		(* MLTREE streams *)
  structure CPs = ClientPseudoOps
  structure PB = PseudoOpsBasisTyp
  structure An = MLRiscAnnotations
  structure CB = CellsBasis

  structure ArgP =              (* Argument passing *)
    ArgPassing(structure Cells=Cells
               structure C=C
               structure MS=MachineSpec)

  structure Frag = Frag(M)      (* Decompose a compilation unit into clusters *)

  structure MemAliasing = MemAliasing(Cells) (* Memory aliasing *)

  structure CPSCCalls =    (* C-Calls handling *)
     CPSCCalls(structure MS = MachineSpec
               structure C  = C
               structure MLTreeComp = MLTreeComp
               structure Cells = Cells
               structure CCalls = CCalls
              )

  fun error msg = MLRiscErrorMsg.error("MLRiscGen", msg)

  (*
   * Debugging
   *)
  fun printCPSFun cps =
      (Control.Print.say "*********************************************** \n";
       PPCps.printcps0 cps;
       Control.Print.say "*********************************************** \n";
       Control.Print.flush()
      )
  val print = Control.Print.say


  (*
   * GC Safety
   *)
  structure GCCells =           (* How to annotate GC information *)
      GCCells(structure C = Cells
              structure GC = SMLGCType)

  val I31    = SMLGCType.I31     (* tagged integers *)
  val I32    = SMLGCType.I32     (* untagged integers *)
  val REAL64 = SMLGCType.REAL64  (* untagged floats *)
  val PTR    = SMLGCType.PTR     (* boxed objects *)
  val NO_OPT = [#create An.NO_OPTIMIZATION ()]

  val enterGC = GCCells.setGCType

  fun sameRegAs x y = CB.sameCell (x, y)

  val ptr = #create An.MARK_REG(fn r => enterGC(r,PTR))
  val i32 = #create An.MARK_REG(fn r => enterGC(r,I32))
  val i31 = #create An.MARK_REG(fn r => enterGC(r,I31))
  val flt = #create An.MARK_REG(fn r => enterGC(r,REAL64))
  fun ctyToAnn CPS.INTt   = i31
    | ctyToAnn CPS.INT32t = i32
    | ctyToAnn CPS.FLTt   = flt
    | ctyToAnn _          = ptr

  (*
   * Convert kind to gc type
   *)
  fun kindToGCty(CPS.P.INT 31) = I31
    | kindToGCty(CPS.P.UINT 31) = I31
    | kindToGCty(_) = I32

  fun ctyToGCty(CPS.FLTt)   = REAL64
    | ctyToGCty(CPS.INTt)   = I31
    | ctyToGCty(CPS.INT32t) = I32
    | ctyToGCty _           = PTR

  (*
   * Make a GC livein/liveout annotation
   *)
  fun gcAnnotation(an, args, ctys) =
  let fun collect(M.GPR(M.REG(_,r))::args,cty::ctys,gctys) =
            collect(args,ctys,(r,ctyToGCty cty)::gctys)
        | collect(M.FPR(M.FREG(_,r))::args,cty::ctys,gctys) =
            collect(args,ctys,(r,ctyToGCty cty)::gctys)
        | collect(_::args,_::ctys,gctys) = collect(args,ctys,gctys)
        | collect([], [], gctys) = gctys
        | collect _ = error "gcAnnotation"
  in  an(collect(args, ctys, [])) end

  (*
   * These are the type widths of ML.  They are hardwired for now.
   *)
  val pty = MS.wordBitWidth (* size of ML's pointer *)
  val ity = MS.wordBitWidth (* size of ML's integer *)
  val fty = 64 (* size of ML's real number *)
  val ws = MS.wordByteWidth

  val zero = M.LI 0
  val one  = M.LI 1
  val two  = M.LI 2
  val mlZero = one (* tagged zero *)
  val offp0 = CPS.OFFp 0
  fun LI i = M.LI (M.I.fromInt(ity, i))
  fun LW w = M.LI (M.I.fromWord32(ity, w))
  val constBaseRegOffset = LI MachineSpec.constBaseRegOffset

  (*
   * The allocation pointer.  This must be a register
   *)
  val allocptrR =
      case C.allocptr of
	  M.REG(_,allocptrR) => allocptrR
	| _ => error "allocptrR"

  (*
   * Dedicated registers.
   *)
  val dedicated' =
    map (fn r => M.GPR(M.REG(ity,r))) C.dedicatedR @
    map (fn f => M.FPR(M.FREG(fty,f))) C.dedicatedF

  val dedicated =
    case C.exhausted of NONE => dedicated'
                      | SOME cc => M.CCR cc :: dedicated'

  (*
   * This flag controls whether extra MLRISC optimizations should be
   * performed.  By default, this is off.
   *)
  val mlrisc   = Control.MLRISC.mkFlag ("mlrisc", "whether to do MLRISC optimizations")

  (*
   * If this flag is on then annotate the registers with GC type info.
   * Otherwise use the default behavior.
   *)
  val gctypes  = Control.MLRISC.mkFlag ("mlrisc-gc-types", "whether to use GC type info")

  (*
   * If this flag is on then perform optimizations before generating gc code.
   * If this flag is on then gctypes must also be turned on!
   * Otherwise use the default behavior.
   *)
  val gcsafety = Control.MLRISC.mkFlag ("mlrisc-gcsafety",
					"whether to optimize before generating GC code")

  (*
   * If this flag is on then split the entry block.
   * This should be on for SSA optimizations.
   *)
  val splitEntry = Control.MLRISC.mkFlag ("split-entry-block", "whether to split entry block")

  (*
   * This dummy annotation is used to get an empty block
   *)
  val EMPTY_BLOCK = #create An.EMPTY_BLOCK ()

  (*
   * convert object descriptor to int
   *)
  val dtoi = LargeWord.toInt

  val newLabel = Label.anon

  (*
   * The main codegen function.
   *)
  fun codegen args =
  let
      val { funcs : CPS.function list,
	    limits:CPS.lvar -> (int*int),
	    err, source } =
	  args
      val maxAlloc = #1 o limits
      val splitEntry = !splitEntry

      (*
       * The natural address arithmetic width of the architecture.
       *)
      val addrTy = MachineSpec.addressBitWidth

      (*
       * These functions generate new virtual register names and
       * mark expressions with their gc types.
       * When the gc-safety feature is turned on, we'll use the
       * versions of newReg that automatically update the GCMap.
       * Otherwise, we'll just use the normal version.
       *)
      val gctypes = !gctypes

      val (newReg, newRegWithCty, newRegWithKind, newFreg)  =
           if gctypes then
              let val newReg  = GCCells.newCell CB.GP
                  val newFreg = GCCells.newCell CB.FP
                  fun newRegWithCty cty = newReg(ctyToGCty cty)
                  fun newRegWithKind kind = newReg(kindToGCty kind)
              in  (newReg, newRegWithCty, newRegWithKind, newFreg) end
           else (Cells.newReg, Cells.newReg, Cells.newReg, Cells.newFreg)

      fun markPTR e = if gctypes then M.MARK(e,ptr) else e
      fun markI32 e = if gctypes then M.MARK(e,i32) else e
      fun markFLT e = if gctypes then M.FMARK(e,flt) else e
      fun markGC(e,cty) = if gctypes then M.MARK(e,ctyToAnn cty) else e
      fun markNothing e = e

      (*
       * Known functions have parameters passed in fresh temporaries.
       * We also annotate the gc types of these temporaries.
       *)
      fun known [] = []
        | known(cty::rest) =
            (case cty of
              CPS.FLTt   => M.FPR(M.FREG(fty,newFreg REAL64))
            | CPS.INTt   => M.GPR(M.REG(ity,newReg I31))
            | CPS.INT32t => M.GPR(M.REG(ity,newReg I32))
            | _          => M.GPR(M.REG(pty,newReg PTR))
            )::known rest

      (*
       * labelTbl is a mapping of function names (CPS.lvars) to labels.
       * If the flag splitEntry is on, we also distinguish between external and
       * internal labels, make sure that no directly branches go to the
       * external labels.
       *)
      exception LabelBind and TypTbl
      val labelTbl : Label.label IntHashTable.hash_table =
	  IntHashTable.mkTable(32, LabelBind)
      val functionLabel = IntHashTable.lookup labelTbl
      val addLabelTbl = IntHashTable.insert labelTbl

      (*
       * typTbl is a mapping of CPS.lvars to CPS types
       *)
      val typTbl  : CPS.cty IntHashTable.hash_table =
	  IntHashTable.mkTable(32, TypTbl)
      val addTypBinding = IntHashTable.insert typTbl
      val typmap = IntHashTable.lookup typTbl

      (*
       * mkGlobalTables define the labels and cty for all CPS functions
       *)
      fun mkGlobalTables(fk, f, _, _, _) =
          ((* internal label *)
           addLabelTbl (f, newLabel());
           (* external entry label *)
           if splitEntry then
             (case fk of
                (CPS.CONT | CPS.ESCAPE) =>
                    addLabelTbl (~f-1, Label.label(Int.toString f) ())
              | _ => ()
             )
           else ();
           case fk
               of CPS.CONT => addTypBinding(f, CPS.CNTt)
            | _ => addTypBinding(f, CPS.BOGt)
           (*esac*))

      val brProb = CpsBranchProb.branchProb funcs

      fun branchWithProb(br, NONE) = br
	| branchWithProb(br, SOME prob) =
	   M.ANNOTATION(br, #create MLRiscAnnotations.BRANCH_PROB prob)

      (*
       * A CPS register may be implemented as a physical
       * register or a memory location.  The function assign moves a
       * value v into a register or a memory location.
       *)
      fun assign(M.REG(ty,r), v) = M.MV(ty, r, v)
	| assign(M.LOAD(ty, ea, mem), v) = M.STORE(ty, ea, v, mem)
	| assign _ = error "assign"


      (*
       * Function for generating code for one cluster.
       *)
      fun genCluster(cluster) = let
          val _ = if !Control.debugging then app PPCps.printcps0 cluster else ()

	  (*
	   * The mltree stream
	   *)
	  val stream as TS.S.STREAM
	    { beginCluster,  (* start a cluster *)
	      endCluster,    (* end a cluster *)
	      emit,          (* emit MLTREE stm *)
	      defineLabel,   (* define a local label *)
	      entryLabel,    (* define an external entry *)
	      exitBlock,     (* mark the end of a procedure *)
	      pseudoOp,      (* emit a pseudo op *)
	      annotation,    (* add an annotation *)
	      ...
	     } = MLTreeComp.selectInstructions (Flowgen.build ())

	 (*
	  * If RCC is present we need to use the virtual frame pointer
	  *)
	  local
	    fun hasRCC([]) = false
	      | hasRCC((_,_,_,_,cexp)::rest) =
		CPS.hasRCC(cexp) orelse hasRCC(rest)
          in
	    val vfp = not MS.framePtrNeverVirtual andalso hasRCC(cluster)
	    val _ = ClusterAnnotation.useVfp := vfp
          end

	  (*
	   * This is the GC comparison test used.  We have a choice of signed
	   * and unsigned comparisons.  This usually doesn't matter, but some
	   * architectures work better in one way or the other, so we are given
	   * a choice here.   For example, the Alpha has to do extra for unsigned
	   * tests, so on the Alpha we use signed tests.
	   *)
	  val gcTest = M.CMP(pty, if C.signedGCTest then M.GT else M.GTU,
			     C.allocptr, C.limitptr(vfp))

          val clusterSize = length cluster

          (* per-cluster tables *)
          exception RegMap and GenTbl

          (*
           * genTbl -- is used to retrieve the parameter passing
           * conventions once a function has been compiled.
           *)
          val genTbl : Frag.frag IntHashTable.hash_table =
	      IntHashTable.mkTable(clusterSize, GenTbl)
          val addGenTbl = IntHashTable.insert genTbl
          val lookupGenTbl = IntHashTable.lookup genTbl

          (*
           * {fp,gp}RegTbl -- mapping of lvars to registers
           *)
          val fpRegTbl : M.fexp IntHashTable.hash_table =
	      IntHashTable.mkTable(2, RegMap)
          val gpRegTbl : M.rexp IntHashTable.hash_table =
	      IntHashTable.mkTable(32, RegMap)
          val addExpBinding = IntHashTable.insert gpRegTbl
          fun addRegBinding(x,r) = addExpBinding(x,M.REG(ity,r))
          val addFregBinding = IntHashTable.insert fpRegTbl

          (*
           * The following function is used to translate CPS into
           * larger trees.  Definitions marked TREEIFY can be forward
           * propagated to their (only) use.   This can drastically reduce
           * register pressure.
           *)
          datatype treeify = TREEIFY | TREEIFIED | COMPUTE | DEAD
          exception UseCntTbl
          val useCntTbl : treeify IntHashTable.hash_table =
	      IntHashTable.mkTable(32, UseCntTbl)
          fun treeify i = getOpt (IntHashTable.find useCntTbl i, DEAD)
          val addCntTbl = IntHashTable.insert useCntTbl
          fun markAsTreeified r = addCntTbl(r, TREEIFIED)
          (*
           * Reset the bindings and use count tables. These tables
           * can be reset at the same time.
           *)
          fun clearTables() =
              (IntHashTable.clear gpRegTbl;
               IntHashTable.clear fpRegTbl;
               IntHashTable.clear useCntTbl
              )

          (*
           * memDisambiguation uses the new register counters,
           * so this must be reset here.
           *)
          val _ = Cells.reset()
          val memDisambig = MemAliasing.analyze(cluster)

          (*
           * Points-to analysis projection.
           *)
          fun pi(x as ref(PT.TOP _),_) = x
            | pi(x,i) = PT.pi(x,i)

          val memDisambigFlag = !CG.memDisambiguate

          fun getRegion e =
              if memDisambigFlag then
                 (case e of
                    CPS.VAR v => memDisambig v
                  | _ => R.readonly
                 )
              else R.memory

          fun getRegionPi(e,i) =
              if memDisambigFlag then
                 (case e of
                    CPS.VAR v => pi(memDisambig v,i)
                  | _ => R.readonly
                 )
              else R.memory

          fun dataptrRegion v = getRegionPi(v, 0)

          (* fun arrayRegion(x as ref(PT.TOP _)) = x
            | arrayRegion x = PT.weakSubscript x *)
          (* For safety, let's assume it's the global memory right now *)
          fun arrayRegion _ = R.memory

          (* This keeps track of all the advanced offset on the hp
           * since the beginning of the CPS function.
           * This is important for generating the correct address offset
           * for newly allocated records.
           *)
          val advancedHP = ref 0

          (*
           * Function grabty lookups the CPS type of a value expression in CPS.
           *)
          fun grabty(CPS.VAR v) = typmap v
            | grabty(CPS.LABEL v) = typmap v
            | grabty(CPS.INT _) = CPS.INTt
            | grabty(CPS.INT32 _) = CPS.INT32t
            | grabty(CPS.VOID) = CPS.FLTt
            | grabty _ = CPS.BOGt

          (*
           * The baseptr contains the start address of the entire
           * compilation unit.  This function generates the address of
           * a label that is embedded in the same compilation unit.  The
           * generated address is relative to the baseptr.
           *
           * Note: For GC safety, we considered this to be an object reference
           *)
          fun laddr(lab, k) =
          let val e =
              M.ADD(addrTy, C.baseptr(vfp),
                    M.LABEXP(M.ADD(addrTy,M.LABEL lab,
                             M.LI(IntInf.fromInt
                                  (k-MachineSpec.constBaseRegOffset)))))
          in  markPTR e end

          (*
           * The following function looks up the MLTREE expression associated
           * with a general purpose value expression.
           *)
          val lookupGpRegTbl = IntHashTable.lookup gpRegTbl

          (*
           * This function resolve the address computation of the
           * form M.CONST k, where offset is a reference to the
           * kth byte allocated since the beginning of the CPS function.
           *)
          fun resolveHpOffset(M.CONST(absoluteHpOffset)) =
              let val tmpR = newReg PTR
                  val offset = absoluteHpOffset - !advancedHP
              in  emit(M.MV(pty, tmpR, M.ADD(addrTy, C.allocptr, LI offset)));
                  M.REG(pty, tmpR)
              end
            | resolveHpOffset(e) = e

          fun regbind(CPS.VAR v) = resolveHpOffset(lookupGpRegTbl v)
            | regbind(CPS.INT i) = LI (i+i+1)
            | regbind(CPS.INT32 w) = LW w
            | regbind(CPS.LABEL v) =
                  laddr(functionLabel(if splitEntry then ~v-1 else v), 0)
            | regbind _ = error "regbind"

          (*
           * This version allows the value to be further propagated
           *)
          fun resolveHpOffset'(M.CONST(absoluteHpOffset)) =
              let val offset = absoluteHpOffset - !advancedHP
              in  markPTR(M.ADD(addrTy, C.allocptr, LI offset))
              end
            | resolveHpOffset'(e) = e

          fun regbind'(CPS.VAR v) = resolveHpOffset'(lookupGpRegTbl v)
            | regbind'(CPS.INT i) = LI (i+i+1)
            | regbind'(CPS.INT32 w) = LW w
            | regbind'(CPS.LABEL v) =
                  laddr(functionLabel(if splitEntry then ~v-1 else v), 0)
            | regbind' _ = error "regbind'"


          (*
           * The following function looks up the MLTREE expression associated
           * with a floating point value expression.
           *)
          val lookupFpRegTbl = IntHashTable.lookup fpRegTbl
          fun fregbind(CPS.VAR v) = lookupFpRegTbl v
            | fregbind _ = error "fregbind"

          (*   On entry to a function, the parameters will be in formal
           * parameter passing registers. Within the body of the function, they
           * are moved immediately to fresh temporary registers. This ensures
           * that the life time of the formal paramters is restricted to the
           * function body and is critical in avoiding artificial register
           * interferences.
           *)
          fun initialRegBindingsEscaping(vl, rl, tl) =
          let fun eCopy(x::xs, M.GPR(M.REG(_,r))::rl, rds, rss, xs', rl') =
                  let val t = newReg PTR
                  in  addRegBinding(x, t);
                      eCopy(xs, rl, t::rds, r::rss, xs', rl')
                  end
                | eCopy(x::xs, r::rl, rds, rss, xs', rl') =
                    eCopy(xs, rl, rds, rss, x::xs', r::rl')
                | eCopy([], [], [], [], xs', rl') = (xs', rl')
                | eCopy([], [], rds, rss, xs', rl') =
                   (emit(M.COPY(ity, rds, rss)); (xs', rl'))
		| eCopy (([], _::_, _, _, _, _) | (_::_, [], _, _, _, _)) =
		    error "eCopy"

              fun eOther(x::xs, M.GPR(r)::rl, xs', rl') =
                  let val t = newReg PTR
                  in  addRegBinding(x, t); emit(M.MV(ity, t, r));
                      eOther(xs, rl, xs', rl')
                  end
                | eOther(x::xs, (M.FPR(M.FREG(_,f)))::rl, xs', rl') =
                    eOther(xs, rl, x::xs', f::rl')
                | eOther([], [], xs, rl) = (xs, rl)
		| eOther (_, M.FPR _ :: _, _, _) =
		    error "eOther: FPR but not FREG"
		| eOther (_, M.CCR _ :: _, _, _) =
		    error "eOther: CCR"
		| eOther (([], _::_, _, _) | (_::_, [], _, _)) =
		    error "eOther"

              fun eFcopy([], []) = ()
                | eFcopy(xs, rl) =
                  let val fs = map (fn _ => newFreg REAL64) xs
                  in  ListPair.app
                        (fn (x,f) => addFregBinding(x,M.FREG(fty,f))) (xs,fs);
                      emit(M.FCOPY(fty, fs, rl))
                  end
              val (vl', rl') = eCopy(vl, rl, [], [], [], [])
          in  eFcopy(eOther(vl', rl', [], []));
              ListPair.app addTypBinding (vl, tl)
          end

          fun initialRegBindingsKnown(vl, rl, tl) =
          let fun f(v, M.GPR(reg as M.REG _)) = addExpBinding(v, reg)
                | f(v, M.FPR(freg as M.FREG _)) = addFregBinding(v, freg)
                | f _ = error "initialRegBindingsKnown.f"
          in  ListPair.app f (vl, rl);
              ListPair.app addTypBinding (vl, tl)
          end

         (* Keep allocation pointer aligned on odd boundary
          * Note: We have accounted for the extra space this eats up in
          *    limit.sml
          *)

          fun updtHeapPtr(hp) =
          let fun advBy hp =
               (advancedHP := !advancedHP + hp;
                emit(M.MV(pty, allocptrR, M.ADD(addrTy, C.allocptr, LI hp))))
          in  if hp = 0 then ()
              else if Word.andb(Word.fromInt hp, Word.fromInt ws) <> 0w0 then advBy(hp+ws)
              else advBy(hp)
          end

          fun testLimit hp =
          let fun assignCC(M.CC(_, cc), v) = emit(M.CCMV(cc, v))
                | assignCC _ = error "testLimit.assign"
          in  updtHeapPtr(hp);
              case C.exhausted
              of NONE => ()
               | SOME cc => assignCC(cc, gcTest)
              (*esac*)
          end


          (*
           * Function to allocate an integer record
           *   x <- [descriptor ... fields]
           *)
          fun ea(r, 0) = r
            | ea(r, n) = M.ADD(addrTy, r, LI n)
          fun indexEA(r, 0) = r
            | indexEA(r, n) = M.ADD(addrTy, r, LI(n*ws))

          fun allocRecord(markComp, mem, desc, fields, hp) =
          let fun getField(v, e, CPS.OFFp 0) = e
                | getField(v, e, CPS.OFFp n) = M.ADD(addrTy, e, LI(ws*n))
                | getField(v, e, p) = getPath(getRegion v, e, p)

              and getPath(mem, e, CPS.OFFp n) = indexEA(e, n)
                | getPath(mem, e, CPS.SELp(n, CPS.OFFp 0)) =
                     markComp(M.LOAD(ity, indexEA(e, n), pi(mem, n)))
                | getPath(mem, e, CPS.SELp(n, p)) =
                  let val mem = pi(mem, n)
                  in  getPath(mem, markPTR(M.LOAD(ity, indexEA(e, n), mem)), p)
                  end

              fun storeFields([], hp, elem) = hp
                | storeFields((v, p)::fields, hp, elem) =
                  (emit(M.STORE(ity, M.ADD(addrTy, C.allocptr, LI hp),
                           getField(v, regbind' v, p), pi(mem, elem)));
                   storeFields(fields, hp+ws, elem+1)
                  )

          in  emit(M.STORE(ity, ea(C.allocptr, hp), desc, pi(mem, ~1)));
              storeFields(fields, hp+ws, 0);
              hp+ws
          end

          (*
           * Functions to allocate a floating point record
           *   x <- [descriptor ... fields]
           *)
          fun allocFrecord(mem, desc, fields, hp) =
          let fun fea(r, 0) = r
                | fea(r, n) = M.ADD(addrTy, r, LI(n*8))
              fun fgetField(v, CPS.OFFp 0) = fregbind v
                | fgetField(v, CPS.OFFp _) = error "allocFrecord.fgetField"
                | fgetField(v, p) = fgetPath(getRegion v, regbind' v, p)

              and fgetPath(mem, e, CPS.OFFp _) = error "allocFrecord.fgetPath"
                | fgetPath(mem, e, CPS.SELp(n, CPS.OFFp 0)) =
                     markFLT(M.FLOAD(fty, fea(e, n), pi(mem, n)))
                | fgetPath(mem, e, CPS.SELp(n, p)) =
                  let val mem = pi(mem, n)
                  in  fgetPath(mem, markPTR(M.LOAD(ity, indexEA(e, n), mem)),p)
                  end

              fun fstoreFields([], hp, elem) = hp
                | fstoreFields((v, p)::fields, hp, elem) =
                  (emit(M.FSTORE(fty, M.ADD(addrTy, C.allocptr, LI hp),
                                 fgetField(v, p), pi(mem, elem)));
                   fstoreFields(fields, hp+8, elem+1)
                  )
          in  emit(M.STORE(ity, ea(C.allocptr, hp), desc, pi(mem, ~1)));
              fstoreFields(fields, hp+ws, 0);
              hp+ws
          end

          (* Allocate a header pair for vector or array *)
          fun allocHeaderPair(hdrDesc, mem, dataPtr, len, hp) =
              (emit(M.STORE(ity, ea(C.allocptr, hp), LI hdrDesc,pi(mem,~1)));
               emit(M.STORE(ity, ea(C.allocptr, hp+ws),
                            M.REG(ity,dataPtr),pi(mem, 0)));
               emit(M.STORE(ity, ea(C.allocptr, hp+2*ws), LI(len+len+1),
                            pi(mem, 1)));
               hp+ws
             )

          (*
           * Int 31 tag optimizations.
           * Note: if the tagging scheme changes then we'll have to redo these.
           *)

          fun addTag e   = M.ADD(ity, e, one)
          fun stripTag e = M.SUB(ity, e, one)
          fun orTag e    = M.ORB(ity, e, one)

          fun tag(false, e) = tagUnsigned e
            | tag(true, e) = tagSigned e
          and tagUnsigned e =
              let fun double r = M.ADD(ity,r,r)
              in  case e
                    of M.REG _ => addTag(double e)
                  | _ => let val tmp = newReg PTR (* XXX ??? *)
                         in  M.LET(M.MV(ity, tmp, e),
                                   addTag(double(M.REG(ity,tmp))))
                         end
              end
          and tagSigned e =
              let fun double r = M.ADDT(ity,r,r)
              in  case e
                  of M.REG _ => addTag(double e)
                   | _ => let val tmp = newReg PTR (* XXX ??? *)
                          in  M.LET(M.MV(ity, tmp, e),
                                    addTag(double(M.REG(ity,tmp))))
                          end
              end

          fun untag(true, e) = untagSigned e
            | untag(false, e) = untagUnsigned e
          and untagUnsigned(CPS.INT i) = LI i
            | untagUnsigned v          = M.SRL(ity, regbind v, one)
          and untagSigned(CPS.INT i) = LI i
            | untagSigned v          = M.SRA(ity, regbind v, one)

          (*
           * Integer operators
           *)
          fun int31add(addOp, CPS.INT k, w) = addOp(ity, LI(k+k), regbind w)
            | int31add(addOp, w, v as CPS.INT _) = int31add(addOp, v, w)
            | int31add(addOp, v, w) = addOp(ity,regbind v,stripTag(regbind w))

          fun int31sub(subOp, CPS.INT k, w) = subOp(ity, LI(k+k+2),regbind w)
            | int31sub(subOp, v, CPS.INT k) = subOp(ity, regbind v, LI(k+k))
            | int31sub(subOp, v, w) = addTag(subOp(ity, regbind v, regbind w))

          fun int31xor(CPS.INT k, w) = M.XORB(ity, LI(k+k), regbind w)
            | int31xor(w, v as CPS.INT _) = int31xor (v,w)
            | int31xor(v, w) = addTag (M.XORB(ity, regbind v, regbind w))

          fun int31mul(signed, mulOp, v, w) =
          let fun f(CPS.INT k, CPS.INT j) = (LI(k+k), LI(j))
                | f(CPS.INT k, w) = (untag(signed,w), LI(k+k))
                | f(v, w as CPS.INT _) = f(w, v)
                | f(v, w) = (stripTag(regbind v), untag(signed,w))
              val (v, w) = f(v, w)
          in  addTag(mulOp(ity, v, w))
          end

          fun int31div(signed, drm, v, w) =
          let val (v, w) =
              case (v, w) of
                (CPS.INT k, CPS.INT j) => (LI k, LI j)
              | (CPS.INT k, w) => (LI k, untag(signed, w))
              | (v, CPS.INT k) => (untag(signed, v), LI(k))
              | (v, w) => (untag(signed, v), untag(signed, w))
          in
	      (* The only way a 31-bit div can overflow is when the result gets retagged.
	       * Therefore, we can use M.DIVS instead of M.DIVT. *)
	      tag (signed,
                   if signed then M.DIVS (drm, ity, v, w)
		   else M.DIVU (ity, v, w))
          end

	  fun int31rem(signed, drm, v, w) = let
              val (v, w) = case (v, w) of
			       (CPS.INT k, CPS.INT j) => (LI k, LI j)
			     | (CPS.INT k, w) => (LI k, untag(signed, w))
			     | (v, CPS.INT k) => (untag(signed, v), LI(k))
			     | (v, w) => (untag(signed, v), untag(signed, w))
	  in
	      tag (false,		(* will not overflow, so we tag like unsigned *)
		   if signed then M.REMS (drm, ity, v, w)
		   else M.REMU (ity, v, w))
	  end

          fun int31lshift(CPS.INT k, w) =
                addTag (M.SLL(ity, LI(k+k), untagUnsigned(w)))
            | int31lshift(v, CPS.INT k) =
                addTag(M.SLL(ity,stripTag(regbind v), LI(k)))
            | int31lshift(v,w) =
                addTag(M.SLL(ity,stripTag(regbind v), untagUnsigned(w)))

          fun int31rshift(rshiftOp, v, CPS.INT k) =
                orTag(rshiftOp(ity, regbind v, LI(k)))
            | int31rshift(rshiftOp, v, w) =
                orTag(rshiftOp(ity, regbind v, untagUnsigned(w)))

          fun getObjDescriptor(v) =
            M.LOAD(ity, M.SUB(pty, regbind v, LI ws), getRegionPi(v, ~1))

          fun getObjLength(v) =
            M.SRL(ity, getObjDescriptor(v), LI(D.tagWidth -1))

          (*
           * Note: because formals are moved into fresh temporaries,
           * (formals intersection actuals) is empty.
           *
           * Do the treeified computation first so as to prevent extra
           * interferences from being created.
           *
           *)
          fun callSetup(formals, actuals) =
          let fun isTreeified(CPS.VAR r) = treeify r = TREEIFIED
                | isTreeified _ = false
              fun gather([], [], cpRd, cpRs, fcopies, treeified, moves) =
                (app emit treeified;
                 case (cpRd,cpRs)
                   of ([],[]) => ()
                    | _ => emit(M.COPY(ity, cpRd, cpRs));
                 case fcopies
                   of [] => ()
                    | _ => emit(M.FCOPY(fty, map #1 fcopies, map #2 fcopies));
                 app emit moves
                )
              | gather(M.GPR(M.REG(ty,rd))::fmls,act::acts,cpRd,cpRs,f,t,m) =
                (case regbind act
                   of M.REG(_,rs) => gather(fmls,acts,rd::cpRd,rs::cpRs,f,t,m)
                    | e => if isTreeified act then
                              gather(fmls, acts, cpRd, cpRs, f,
                                     M.MV(ty, rd, e)::t, m)
                           else
                              gather(fmls, acts, cpRd, cpRs, f,
                                     t, M.MV(ty, rd, e)::m)
                 (*esac*))
              | gather(M.GPR(M.LOAD(ty,ea,r))::fmls,act::acts,cpRd,cpRs,f,t,m) =
                  (* Always store them early! *)
                  gather(fmls,acts,cpRd,cpRs,f,
                         M.STORE(ty,ea,regbind act,r)::t, m)
              | gather(M.FPR(M.FREG(ty,fd))::fmls,act::acts,cpRd,cpRs,f,t,m) =
                (case fregbind act
                   of M.FREG(_,fs) =>
                        gather(fmls,acts,cpRd,cpRs,(fd,fs)::f,t,m)
                    | e =>
                        if isTreeified act then
                           gather(fmls,acts,cpRd,cpRs,f,M.FMV(ty, fd, e)::t,m)
                        else
                           gather(fmls,acts,cpRd,cpRs,f,t,M.FMV(ty, fd, e)::m)
                 (*esac*))
              | gather _ = error "callSetup.gather"
          in  gather(formals, actuals, [], [], [], [], [])
          end

          (* scale-and-add *)
          fun scale1(a, CPS.INT 0) = a
            | scale1(a, CPS.INT k) = M.ADD(ity, a, LI(k))
            | scale1(a, i) = M.ADD(ity, a, untagSigned(i))

          fun scale4(a, CPS.INT 0) = a
            | scale4(a, CPS.INT i) = M.ADD(ity, a, LI(i*4))
            | scale4(a, i) = M.ADD(ity, a, M.SLL(ity, untagSigned(i), two))


          fun scale8(a, CPS.INT 0) = a
            | scale8(a, CPS.INT i) = M.ADD(ity, a, LI(i*8))
            | scale8(a, i) = M.ADD(ity, a, M.SLL(ity, stripTag(regbind i),
                                                  LI(2)))

          val scaleWord = (case ws
		 of 4 => scale4
		  | 8 => scale8
		  | _ => error "scaleWord"
		(* end case *))

 	  (* zero-extend and sign-extend *)
	  fun ZX32 (sz, e) = M.ZX (32, sz, e)
	      (* M.SRL (32, M.SLL (32, e, LI (32 - sz)), LI (32 - sz)) *)
	  fun SX32 (sz, e) = M.SX (32, sz, e)
	      (* M.SRA (32, M.SLL (32, e, LI (32 - sz)), LI (32 - sz)) *)

          (* add to storelist, the address where a boxed update has occured *)
          fun recordStore(tmp, hp) =
            (emit(M.STORE(pty,M.ADD(addrTy,C.allocptr,LI(hp)),
                                    tmp,R.storelist));
             emit(M.STORE(pty,M.ADD(addrTy,C.allocptr,LI(hp+ws)),
                                    C.storeptr(vfp),R.storelist));
             emit(assign(C.storeptr(vfp), M.ADD(addrTy, C.allocptr, LI(hp)))))

          fun unsignedCmp oper =
              case oper
                of P.>   => M.GTU | P.>=  => M.GEU
                 | P.<   => M.LTU | P.<=  => M.LEU
                 | P.eql => M.EQ  | P.neq => M.NE

          fun signedCmp oper =
              case oper
                of P.>   => M.GT | P.>=  => M.GE
                 | P.<   => M.LT | P.<=  => M.LE
                 | P.neq => M.NE | P.eql => M.EQ

          fun real64Cmp(oper, v, w) =
          let  val fcond =
                      case oper
                        of P.fEQ => M.==
                         | P.fULG => M.?<>
                         | P.fUN => M.?
                         | P.fLEG => M.<=>
                         | P.fGT => M.>
                         | P.fGE  => M.>=
                         | P.fUGT => M.?>
                         | P.fUGE => M.?>=
                         | P.fLT => M.<
                         | P.fLE  => M.<=
                         | P.fULT => M.?<
                         | P.fULE => M.?<=
                         | P.fLG => M.<>
                         | P.fUE  => M.?=
			 | P.fsgn => error "unary fsgn used as binary operator"
          in M.FCMP(64, fcond, fregbind v, fregbind w) end

          fun branchToLabel(lab) = M.JMP(M.LABEL lab,[])

          local
            open CPS
          in

          (*
           * This function initializes a CPS function before we generate
           * code for it.   Its tasks include:
           * 1. Add type bindings for each definition. This is used to determine
           *    the parameter passing convention for standard functions.
           * 2. Compute the number of uses for each variable.  This is
           *    used in the forward propagation logic.
           * 3. Check whether the base pointer is needed.
           *      It is needed iff
           *       a.  There is a reference to LABEL
           *       b.  It uses SWITCH (the jumptable requires the basepointer)
           * 4. Generate the gc tests for STANDARD and KNOWN functions
           * 5. Check to see if floating point allocation is being performed
           *    in the function.  If so, we will align the allocptr.
           *)
          fun genCPSFunction(lab, kind, f, params, formals, tys, e) =
          let val add = addTypBinding
              fun addUse v =
                  case treeify v of
                    DEAD => addCntTbl(v, TREEIFY)
                  | TREEIFY => addCntTbl(v, COMPUTE)
                  | COMPUTE => ()
                  | _ => error "addUse"

              val hasFloats = ref false (* default is no *)
              val needBasePtr = ref false

              fun addValue(VAR v) = addUse v
                | addValue(LABEL _) = needBasePtr := true
                | addValue _ = ()

              fun addValues [] = ()
                | addValues(VAR v::vs) = (addUse v; addValues vs)
                | addValues(LABEL _::vs) = (needBasePtr := true; addValues vs)
                | addValues(_::vs) = addValues vs

              fun addRecValues [] = ()
                | addRecValues((VAR v,_)::l) = (addUse v; addRecValues l)
                | addRecValues((LABEL v,_)::l) =
                   (needBasePtr := true; addRecValues l)
                | addRecValues(_::l) = addRecValues l

              fun init e =
              case e
              of RECORD(k,vl,x,e) =>
                   (case k of
                      (RK_FCONT | RK_FBLOCK) => hasFloats := true
                    | _ => ();
                    addRecValues vl; add(x,BOGt); init e
                   )
               | SELECT(_,v,x,t,e) => (addValue v; add(x,t); init e)
               | OFFSET(_,v,x,e) => (addValue v; add(x,BOGt); init e)
               | SWITCH(v,_,el) =>
                   (needBasePtr := true; addValue v; app init el)
               | SETTER(_,vl,e) => (addValues vl; init e)
               | LOOKER(looker,vl,x,t,e) =>
                    (addValues vl;
                    (* floating subscript cannot move past a floating update.
                     * For now subscript operations cannot be treeified.
                     * This is hacked by making it (falsely) used
                     * more than once.
                     *)
                     case looker of
                       (P.numsubscript{kind=P.FLOAT _} |
                        P.rawload {kind=P.FLOAT _}) =>
                       addCntTbl(x,COMPUTE)
                     | _ => ();
                     add(x,t); init e
                    )
               | ARITH(_,vl,x,t,e) => (addValues vl; add(x,t); init e)
               | RCC(_,_,_,vl,wl,e) => (addValues vl; app add wl; init e)
               | PURE(p,vl,x,t,e) =>
                    (case p of
                       P.fwrap => hasFloats := true
                     | _ => ();
                     addValues vl; add(x,t); init e
                    )
               | BRANCH(_,vl,_,e1,e2) => (addValues vl; init e1; init e2)
               | APP(v,vl) => (addValue v; addValues vl)
               | _ => error "genCPSFunction"

          in  (* Print debugging information *)
              if !CG.printit then printCPSFun(kind,f,params,tys,e) else ();

              (* Move parameters *)
              case kind of
                KNOWN =>
                   (defineLabel lab;
                    init e;
                    initialRegBindingsEscaping(params, formals, tys)
                   )
              | KNOWN_CHECK =>
                   (defineLabel lab;
                    (* gc test *)
                    (if !mlrisc andalso !gcsafety then
                     InvokeGC.optimizedKnwCheckLimit else
                     InvokeGC.knwCheckLimit)
                        stream
                        {maxAlloc=4*maxAlloc f, regfmls=formals, regtys=tys,
                         return=branchToLabel(lab)};
                    init e;
                    initialRegBindingsEscaping(params, formals, tys)
                   )
              | _ =>
                 (* Standard function *)
                 let val regfmls = formals
		     val (linkreg, regfmlsTl) =
			 case formals of
			     (M.GPR linkreg::regfmlsTl) => (linkreg, regfmlsTl)
			   | _ => error "no linkreg for standard function"
                     val entryLab =
                         if splitEntry then functionLabel(~f-1) else lab
                 in
                     if splitEntry then
                      (entryLabel entryLab;
                       annotation EMPTY_BLOCK;
                       defineLabel lab
                      )
                     else
                      entryLabel lab;
                     clearTables();
                     init e;
                     if !needBasePtr then
                       let val baseval =
                             M.ADD(addrTy,linkreg,
                                   M.LABEXP(M.SUB(addrTy,
                                       constBaseRegOffset,
                                       M.LABEL entryLab)))
                       in  emit(assign(C.baseptr(vfp), baseval)) end
                     else ();
                     InvokeGC.stdCheckLimit stream
                         {maxAlloc=4 * maxAlloc f, regfmls=regfmls,
                          regtys=tys, return=M.JMP(linkreg,[])};
                     initialRegBindingsEscaping
                       (List.tl params, regfmlsTl, List.tl tys)
                 end
              ;

              (* Align the allocation pointer if necessary *)
              if !hasFloats then
                 emit(M.MV(pty,allocptrR, M.ORB(pty,C.allocptr, LI ws)))
              else ();

              (* Generate code *)
              advancedHP := 0;
              gen(e, 0)
(*+DEBUG*)
handle ex => (
print "***** exception\n";
printCPSFun(kind,f,params,tys,e);
raise ex)
(*-DEBUG*)

          end

          (*
           * Generate code for x := e; k
           *)
          and define(r, x, e, k, hp) =
              (addRegBinding(x, r);
               emit(M.MV(ity, r, e));
               gen(k, hp)
              )

          and def(gc, x, e, k, hp) = define(newReg gc,x,e,k,hp)

          and defWithCty(cty, x, e, k, hp) = define(newRegWithCty cty,x,e,k,hp)

          and defWithKind(kind, x, e, k, hp) =
               define(newRegWithKind kind,x,e,k,hp)

          and defI31(x, e, k, hp) = def(I31, x, e, k, hp)
          and defI32(x, e, k, hp) = def(I32, x, e, k, hp)
          and defBoxed(x, e, k, hp) = def(PTR, x, e, k, hp)

          (*
           * Generate code for x : cty := e; k
           *)
          and treeifyDef(x, e, cty, k, hp) =
              case treeify x of
                COMPUTE => defWithCty(cty, x, e, k, hp)
              | TREEIFY => (markAsTreeified x;
                            addExpBinding(x, markGC(e, cty)); gen(k, hp))
              | DEAD    => gen(k, hp)
              | _       => error "treeifyDef"

          (*
           * Generate code for
           *    x := allocptr + offset; k
           * where offset is the address offset of a newly allocated record.
           * If x is only used once, we try to propagate that to its use.
           *)
          and defAlloc(x, offset, k, hp) =
                defBoxed(x, M.ADD(addrTy, C.allocptr, LI offset), k, hp)


          (* Generate code for
           *    x := allocptr + offset; k
           * Forward propagate until it is used.
           *)
          and treeifyAlloc(x, offset, k, hp) =
              (case treeify x of
                COMPUTE => defAlloc(x, offset, k, hp)
              | TREEIFY =>
                (* Note, don't mark this as treeified since it has low
                 * register pressure.
                 *)
                let val absoluteAllocOffset = offset + !advancedHP
                in  addExpBinding(x, M.CONST(absoluteAllocOffset));
                    gen(k, hp)
                end
              | DEAD => gen(k, hp)
              | _    => error "treeifyAlloc"
              )

	  and computef64(x, e, k, hp) = let
	    val f = newFreg REAL64
          in
	    addFregBinding(x, M.FREG(fty, f));
	    emit(M.FMV(fty, f, e));
	    gen(k, hp)
          end
          (*
           * x <- e where e contains an floating-point value
           *)
          and treeifyDefF64(x, e, k, hp) =
             (case treeify x
                of DEAD => gen(k, hp)
                 | TREEIFY => (markAsTreeified x;
                               addFregBinding(x,e); gen(k, hp))
                 | COMPUTE => computef64(x, e, k, hp)
                 | _    => error "treeifyDefF64"
              (*esac*))

          and nop(x, v, e, hp) = defI31(x, regbind v, e, hp)

          and copy(gc, x, v, k, hp) =
          let val dst = newReg gc
          in  addRegBinding(x, dst);
              case regbind v
                of M.REG(_,src) => emit(M.COPY(ity, [dst], [src]))
                 | e => emit(M.MV(ity, dst, e))
              (*esac*);
              gen(k, hp)
          end

          and copyM(31, x, v, k, hp) = copy(I31, x, v, k, hp)
            | copyM(_, x, v, k, hp)  = copy(I32, x, v, k, hp)

          and eqVal(VAR x,VAR y) = x = y
            | eqVal(LABEL x,LABEL y) = x = y
            | eqVal(INT x, INT y) = x = y
            | eqVal _ = false

              (* normal branches *)
          and branch (cv, cmp, [v, w], yes, no, hp) =
              let val trueLab = newLabel ()
              in  (* is single assignment great or what! *)
		  emit
	              (branchWithProb
			(M.BCC(M.CMP(32, cmp, regbind v, regbind w), trueLab),
			 brProb cv));
		      genCont(no, hp);
		      genlab(trueLab, yes, hp)
              end
	    | branch _ = error "branch"

              (* branch if x is boxed *)
          and branchOnBoxed(cv, x, yes, no, hp) =
              let val lab = newLabel()
                  val cmp = M.CMP(32, M.NE, M.ANDB(ity, regbind x, one), zero)
              in
		  emit(branchWithProb(M.BCC(cmp, lab), brProb cv));
                  genCont(yes, hp);
                  genlab(lab, no, hp)
              end

              (* branch if are identical strings v, w of length n *)
          and branchStreq(n, v, w, yes, no, hp) =
              let val n' = ((n+3) div 4) * 4
                  val false_lab = newLabel ()
                  val r1 = newReg I32
                  val r2 = newReg I32
                  fun cmpWord(i) =
                      M.CMP(32, M.NE,
                            M.LOAD(ity,M.ADD(ity,M.REG(ity, r1),i),R.readonly),
                            M.LOAD(ity,M.ADD(ity,M.REG(ity, r2),i),R.readonly))
                  fun unroll i =
                      if i=n' then ()
                      else (emit(M.BCC(cmpWord(LI(i)), false_lab));
                            unroll (i+4))
              in  emit(M.MV(ity, r1, M.LOAD(ity, regbind v, R.readonly)));
                  emit(M.MV(ity, r2, M.LOAD(ity, regbind w, R.readonly)));
                  unroll 0;
                  genCont(yes, hp);
                  genlab(false_lab, no, hp)
              end

              (* conditional move *)
          and condmove(oper, args, x, t, e, hp) =
              let  fun signed(oper, v, w) =
                       M.CMP(32, signedCmp oper, regbind v, regbind w)
                   fun unsigned(oper, v, w) =
                       M.CMP(32, unsignedCmp oper, regbind v, regbind w)
                   fun equal(v, w) =
                       M.CMP(32, M.EQ, regbind v, regbind w)
                   fun notequal(v, w) =
                       M.CMP(32, M.NE, regbind v, regbind w)
                   fun unboxed x =
                       M.CMP(32, M.NE, M.ANDB(ity, regbind x, one), zero)
                   fun boxed x =
                       M.CMP(32, M.EQ, M.ANDB(ity, regbind x, one), zero)
                   val (cmp, a, b) =
                   case (oper, args) of
                     (P.cmp{oper, kind=P.INT 31},[v,w,a,b]) =>
                       (signed(oper,v,w), a, b)
                   | (P.cmp{oper, kind=P.UINT 31},[v,w,a,b]) =>
                       (unsigned(oper,v,w), a, b)
                   | (P.cmp{oper, kind=P.INT 32},[v,w,a,b]) =>
                       (signed(oper,v,w), a, b)
                   | (P.cmp{oper, kind=P.UINT 32},[v,w,a,b]) =>
                       (unsigned(oper,v,w), a, b)
                   | (P.fcmp{oper, size=64},[v,w,a,b]) =>
                       (real64Cmp(oper,v,w), a, b)
                   | (P.peql,[v,w,a,b]) => (equal(v,w), a, b)
                   | (P.pneq,[v,w,a,b]) => (notequal(v, w), a, b)
                   | (P.boxed,[v,a,b]) => (boxed v, a, b)
                   | (P.unboxed,[v,a,b]) => (unboxed v, a, b)
                   | _ => error "condmove"
              in  case t of
                     FLTt =>
                       computef64(x,
                          M.FCOND(64, cmp, fregbind a, fregbind b), e, hp)
                   | _    =>
                       defWithCty(t, x, M.COND(32, cmp, regbind a, regbind b),
                                  e, hp)
              end

          and arith(gc, oper, v, w, x, e, hp) =
               def(gc, x, oper(ity, regbind v, regbind w), e, hp)

          and arith32(oper, v, w, x, e, hp) =
               arith(I32, oper, v, w, x, e, hp)

          and logical(gc, oper, v, w, x, e, hp) =
               def(gc, x, oper(ity, regbind v, untagUnsigned(w)), e, hp)

          and logical31(oper, v, w, x, e, hp) =
               logical(I31, oper, v, w, x, e, hp)

          and logical32(oper, v, w, x, e, hp) =
               logical(I32, oper, v, w, x, e, hp)

          and genCont(e, hp) =
              let val save = !advancedHP
              in  gen(e, hp); advancedHP := save end

          and genlab(lab, e, hp) = (defineLabel lab; gen(e, hp))

          and genlabCont(lab, e, hp) = (defineLabel lab; genCont(e, hp))

             (* Allocate a normal record *)
          and mkRecord(vl, w, e, hp) =
              let val len = length vl
                  val desc = dtoi(D.makeDesc (len, D.tag_record))
              in  treeifyAlloc(w,
                     allocRecord(markPTR, memDisambig w, LI desc, vl, hp),
                        e, hp+ws+len*ws)
              end

             (* Allocate a record with I32 components *)
           and mkI32block(vl, w, e, hp) =
              let val len = length vl
                  val desc = dtoi(D.makeDesc (len, D.tag_raw32))
              in  treeifyAlloc(w,
                     allocRecord(markI32, memDisambig w, LI desc, vl, hp),
                        e, hp+ws+len*ws)
              end

              (* Allocate a floating point record *)
          and mkFblock(vl, w, e, hp) =
              let val len = List.length vl
                  val desc = dtoi(D.makeDesc(len+len, D.tag_raw64))
                (* At initialization the allocation pointer is aligned on
                 * an odd-word boundary, and the heap offset set to zero. If an
                 * odd number of words have been allocated then the heap pointer
                 * is misaligned for this record creation.
                 *)
                  val hp = if ws = 4 andalso Word.andb(Word.fromInt hp, 0w4) <> 0w0
			then hp+4
			else hp
              in  (* The components are floating point *)
                  treeifyAlloc(w,
                     allocFrecord(memDisambig w, LI desc, vl, hp),
                        e, hp+ws+len*8)
              end

              (* Allocate a vector *)
          and mkVector(vl, w, e, hp) =
              let val len = length vl
                  val hdrDesc = dtoi(D.desc_polyvec)
                  val dataDesc = dtoi(D.makeDesc(len, D.tag_vec_data))
                  val dataPtr = newReg PTR
                  val mem     = memDisambig w
                  val hp'     = hp + ws + len*ws
              in  (* The components are boxed *)
                  (* Allocate the data *)
                  allocRecord(markPTR, mem, LI dataDesc, vl, hp);
                  emit(M.MV(pty, dataPtr, ea(C.allocptr, hp+ws)));
                  (* Now allocate the header pair *)
                  treeifyAlloc(w,
                     allocHeaderPair(hdrDesc, mem, dataPtr, len, hp+ws+len*ws),
                        e, hp'+3*ws)
              end

          (*
           * Floating point select
           *)
          and fselect(i, v, x, e, hp) =
              treeifyDefF64(x,
                   M.FLOAD(fty, scale8(regbind v, INT i), R.real),
                            e, hp)

          (*
           * Non-floating point select
           *)
          and select(i, v, x, t, e, hp) =
              treeifyDef(x,
                  M.LOAD(ity,scaleWord(regbind v,INT i),getRegionPi(v,i)),
                         t, e, hp)

          (*
           * Funny select; I don't know that this does
           *)
          and funnySelect(i, k, x, t, e, hp) =
              let val unboxedfloat = MS.unboxedFloats
                  fun isFlt t =
                    if unboxedfloat then (case t of FLTt => true | _ => false)
                    else false
                  fun fallocSp(x,e,hp) =
                    (addFregBinding(x,M.FREG(fty,newFreg REAL64));gen(e, hp))
                 (* warning: the following generated code should never be
                    executed; its semantics is completely screwed up !
                  *)
              in  if isFlt t then fallocSp(x, e, hp)
                  else defI32(x, LI k, e, hp)(* BOGUS *)
              end

          (*
           * Call an external function
           *)
          and externalApp(f, args, hp) =
              let val ctys = map grabty args
                  val formals =
		    ArgP.standard{fnTy=typmap f, vfp=vfp, argTys=ctys}
		  val dest =
		      case formals of
			  (M.GPR dest::_) => dest
			| _ => error "externalApp: dest"
              in  callSetup(formals, args);
                  if gctypes then
                    annotation(gcAnnotation(#create GCCells.GCLIVEOUT,
                                            formals, ctys))
                  else ();
                  testLimit hp;
                  emit(M.JMP(dest, []));
                  exitBlock(formals @ dedicated)
              end

          (*
           * Call an internal function
           *)
          and internalApp(f, args, hp) =
              (case lookupGenTbl f
                of Frag.KNOWNFUN(ref(Frag.GEN formals)) =>
                    (updtHeapPtr(hp);
                     callSetup(formals, args);
                     emit(branchToLabel(functionLabel f)))
                 | Frag.KNOWNFUN(r as ref(Frag.UNGEN(f,vl,tl,e))) =>
                   let val formals = known tl
                       val lab = functionLabel f
                   in  r := Frag.GEN formals;
                       updtHeapPtr(hp);
                       callSetup(formals, args);
                       genCPSFunction(lab, KNOWN, f, vl, formals, tl, e)
                   end
                 | Frag.KNOWNCHK(r as ref(Frag.UNGEN(f,vl,tl,e))) =>
                   let val formals =
                           if MS.fixedArgPassing then ArgP.fixed{argTys=tl, vfp=vfp}
                           else known tl
                       val lab = functionLabel f
                   in  r := Frag.GEN formals;
                       callSetup(formals, args);
                       testLimit hp;
                       genCPSFunction(lab, KNOWN_CHECK, f, vl, formals, tl, e)
                   end
                 | Frag.KNOWNCHK(ref(Frag.GEN formals)) =>
                     (callSetup(formals, args);
                      testLimit hp;
                      emit(branchToLabel(functionLabel f)))
                 | Frag.STANDARD{fmlTyps, ...} =>
                   let val formals = ArgP.standard{fnTy=typmap f, argTys=fmlTyps, vfp=vfp}
                   in  callSetup(formals, args);
                       testLimit hp;
                       emit(branchToLabel(functionLabel f))
                   end
              (*esac*))

	  and rawload ((P.INT 32 | P.UINT 32), i, x, e, hp) =
	      defI32 (x, M.LOAD (32, i, R.memory), e, hp)
	    | rawload ((P.INT 64 | P.UINT 64), i, x, e, hp) =
	      defI32 (x, M.LOAD (64, i, R.memory), e, hp) (* XXX64 not yet *)
	    | rawload (P.INT (sz as (8 | 16)), i, x, e, hp) =
	      defI32 (x, SX32 (sz, M.LOAD (sz, i, R.memory)), e, hp)
	    | rawload (P.UINT (sz as (8 | 16)), i, x, e, hp) =
	      defI32 (x, ZX32 (sz, M.LOAD (sz, i, R.memory)), e, hp)
	    | rawload ((P.UINT sz | P.INT sz), _, _, _, _) =
	      error ("rawload: unsupported size: " ^ Int.toString sz)
	    | rawload (P.FLOAT 64, i, x, e, hp) =
	      treeifyDefF64 (x, M.FLOAD (64, i, R.memory), e, hp)
	    | rawload (P.FLOAT 32, i, x, e, hp) =
	      treeifyDefF64 (x, M.CVTF2F (64, 32, M.FLOAD (32, i, R.memory)),
			     e, hp)
	    | rawload (P.FLOAT sz, _, _, _, _) =
	      error ("rawload: unsupported float size: " ^ Int.toString sz)

	  and rawstore ((P.UINT (sz as (8 | 16 | 32 | 64)) |
			 P.INT (sz as (8 | 16 | 32 | 64))), i, x) =
	      (* both address and value are 32-bit values; only sz bits
	       * of the value are being stored *)
	      emit (M.STORE (sz, i, regbind x, R.memory))
	    | rawstore ((P.UINT sz | P.INT sz), _, _) =
	      error ("rawstore: unsupported int size: " ^ Int.toString sz)
	    | rawstore (P.FLOAT (sz as (32 | 64)) , i, x) =
	      emit (M.FSTORE (sz, i, fregbind x, R.memory))
	    | rawstore (P.FLOAT sz, _, _) =
	      error ("rawstore: unsupported float size: " ^ Int.toString sz)


          (*
           * Generate code
           *)

            (** RECORD **)
          and gen(RECORD(RK_FCONT, vl, w, e), hp) = mkFblock(vl, w, e, hp)
            | gen(RECORD(RK_FBLOCK, vl, w, e), hp) = mkFblock(vl, w, e, hp)
            | gen(RECORD(RK_VECTOR, vl, w, e), hp) = mkVector(vl, w, e, hp)
            | gen(RECORD(RK_I32BLOCK, vl, w, e), hp) = mkI32block(vl, w, e, hp)
            | gen(RECORD(_, vl, w, e), hp) = mkRecord(vl, w, e, hp)

            (*** SELECT ***)
            | gen(SELECT(i, INT k, x, t, e), hp) = funnySelect(i,k,x,t,e,hp)
            | gen(SELECT(i, v, x, FLTt, e), hp) = fselect(i, v, x, e, hp)
            | gen(SELECT(i, v, x, t, e), hp) = select(i, v, x, t, e, hp)

            (*** OFFSET ***)
            | gen(OFFSET(i, v, x, e), hp) =
                 defBoxed(x, scaleWord(regbind v, INT i), e, hp)

            (*** APP ***)
            | gen(APP(INT k, args), hp) = updtHeapPtr(hp)
            | gen(APP(VAR f, args), hp) = externalApp(f, args, hp)
            | gen(APP(LABEL f, args), hp) = internalApp(f, args, hp)

            (*** SWITCH ***)
            | gen(SWITCH(INT _, _, _), hp) = error "SWITCH"
            | gen(SWITCH(v, _, l), hp) =
              let val lab = newLabel ()
                  val labs = map (fn _ => newLabel()) l
                  val tmpR = newReg I32 val tmp = M.REG(ity,tmpR)
              in  emit(M.MV(ity, tmpR, laddr(lab, 0)));
                  emit(M.JMP(M.ADD(addrTy, tmp, M.LOAD(pty, scaleWord(tmp, v),
                                                       R.readonly)), labs));
		  pseudoOp(PB.DATA_READ_ONLY);
		  pseudoOp(PB.EXT(CPs.JUMPTABLE{base=lab, targets=labs}));
		  pseudoOp(PB.TEXT);
                  ListPair.app (fn (lab, e) => genlabCont(lab, e, hp)) (labs, l)
              end

            (*** PURE ***)
            | gen(PURE(P.real{fromkind=P.INT 31, tokind=P.FLOAT 64},
                       [v], x, _, e), hp) =
                treeifyDefF64(x,M.CVTI2F(fty,ity,untagSigned(v)), e, hp)
	    | gen(PURE(P.real{fromkind=P.INT 32, tokind=P.FLOAT 64},
		       [v], x, _, e), hp) =
	        treeifyDefF64(x,M.CVTI2F(fty,ity,regbind v), e, hp)
            | gen(PURE(P.pure_arith{oper, kind=P.FLOAT 64}, [v], x, _, e), hp) = let
                val r = fregbind v
              in
		case oper
                of P.~ => treeifyDefF64(x, M.FNEG(fty,r), e, hp)
                 | P.abs => treeifyDefF64(x, M.FABS(fty,r), e, hp)
		 | P.fsqrt => treeifyDefF64(x, M.FSQRT(fty,r), e, hp)
		 | P.fsin => computef64(x, M.FEXT(fty, E.FSINE r), e, hp)
		 | P.fcos => computef64(x, M.FEXT(fty, E.FCOSINE r), e, hp)
		 | P.ftan => computef64(x, M.FEXT(fty, E.FTANGENT r), e, hp)
		 | _ => error "unexpected primop in pure unary float64"
              end
            | gen(PURE(P.pure_arith{oper, kind=P.FLOAT 64}, [v,w], x, _, e), hp) =
              let val v = fregbind v
                  val w = fregbind w
                  val t =
                  case oper
                    of P.+ => M.FADD(fty, v, w)
                     | P.* => M.FMUL(fty, v, w)
                     | P.- => M.FSUB(fty, v, w)
                     | P./ => M.FDIV(fty, v, w)
		     | _ => error "unexpected primop in pure binary float64"
              in  treeifyDefF64(x, t, e, hp)
              end
            | gen(PURE(P.pure_arith{oper=P.orb, kind}, [v,w], x, _, e), hp) =
                defWithKind(kind, x, M.ORB(ity, regbind v, regbind w), e, hp)
            | gen(PURE(P.pure_arith{oper=P.andb, kind}, [v,w], x, _, e), hp) =
                defWithKind(kind, x, M.ANDB(ity, regbind v, regbind w), e, hp)
            | gen(PURE(P.pure_arith{oper, kind}, [v,w], x, ty, e), hp) =
              (case kind
                of P.INT 31 => (case oper
                     of P.xorb   => defI31(x, int31xor(v,w), e, hp)
                      | P.lshift => defI31(x, int31lshift(v,w), e, hp)
                      | P.rshift => defI31(x, int31rshift(M.SRA,v,w),e,hp)
		      | P.+ => defI31(x, int31add(M.ADD, v, w), e, hp)
		      | P.- => defI31(x, int31sub(M.SUB, v, w), e, hp)
		      | P.* => defI31(x, int31mul(true, M.MULS, v, w), e, hp)
                      | _ => error "gen:PURE INT 31"
                    (*esac*))
                 | P.INT 32  => (case oper
                     of P.xorb  => arith32(M.XORB, v, w, x, e, hp)
                      | P.lshift => logical32(M.SLL, v, w, x, e, hp)
                      | P.rshift => logical32(M.SRA, v, w, x, e, hp)
                      | _ => error "gen:PURE INT 32"
                    (*esac*))
                 | P.UINT 31 => (case oper
                     of P.+    => defI31(x, int31add(M.ADD, v, w), e, hp)
                      | P.-    => defI31(x, int31sub(M.SUB, v, w), e, hp)
                      | P.*    => defI31(x, int31mul(false, M.MULU, v, w),
					 e, hp)
		      (* we now explicitly defend agains div by 0, so these
		       * two can be treated as pure op: *)
		      | P./ => defI31(x,int31div(false,M.DIV_TO_ZERO,v,w),
				      e,hp)
		      | P.rem => defI31(x,int31rem(false,M.DIV_TO_ZERO,v,w),
					e,hp)
(*
                      | P./    => (* This is not really a pure
                                     operation -- oh well *)
                                 (updtHeapPtr hp;
                                  defI31(x, int31div(false, M.DIV_TO_ZERO,
						     v, w),
					 e, 0))
		      | P.rem => (* Neither is this -- oh well *)
			         (updtHeapPtr hp;
				  defI31(x, int31rem(false, M.DIV_TO_ZERO,
						     v, w),
					 e, 0))
*)
                      | P.xorb => defI31(x, int31xor(v, w), e, hp)
                      | P.lshift  => defI31(x,int31lshift(v, w), e, hp)
                      | P.rshift  => defI31(x,int31rshift(M.SRA,v, w),e,hp)
                      | P.rshiftl => defI31(x,int31rshift(M.SRL,v, w),e,hp)
                      | _ => error "gen:PURE UINT 31"
                    (*esac*))
                 | P.UINT 32 => (case oper
                     of P.+     => arith32(M.ADD, v, w, x, e, hp)
                      | P.-     => arith32(M.SUB, v, w, x, e, hp)
                      | P.*     => arith32(M.MULU, v, w, x, e, hp)
		      (* same here: the explicit test for 0 that is inserted
		       * by translate lets us treat the following two as
		       * pure: *)
		      | P./     => arith32(M.DIVU, v, w, x, e, hp)
		      | P.rem   => arith32(M.REMU, v, w, x, e, hp)
(*
                      | P./     => (updtHeapPtr hp;
                                    arith32(M.DIVU, v, w, x, e, 0))
		      | P.rem   => (updtHeapPtr hp;
				    arith32(M.REMU, v, w, x, e, 0))
*)
                      | P.xorb  => arith32(M.XORB, v, w, x, e, hp)
                      | P.lshift => logical32(M.SLL, v, w, x, e, hp)
                      | P.rshift => logical32(M.SRA, v, w, x, e, hp)
                      | P.rshiftl=> logical32(M.SRL, v, w, x, e, hp)
                      | _ => error "gen:PURE UINT 32"
                    (*esac*))
		 | _ => error "unexpected numkind in pure binary arithop"
              (*esac*))
            | gen(PURE(P.pure_arith{oper=P.notb, kind}, [v], x, _, e), hp) =
               (case kind
                of (P.UINT 32 | P.INT 32) =>
		     defI32(x,M.XORB(ity, regbind v,  LW 0wxFFFFFFFF), e, hp)
                 | (P.UINT 31 | P.INT 31) =>
		     defI31(x,M.SUB(ity, zero, regbind v), e, hp)
		 | _ => error "unexpected numkind in pure notb arithop"
              (*esac*))
	    | gen (PURE(P.pure_arith{oper=P.~, kind}, [v], x, _, e), hp) =
	        (case kind of
		     (P.UINT 32 | P.INT 32) =>
		       defI32 (x, M.SUB(ity, zero, regbind v), e, hp)
		   | (P.UINT 31 | P.INT 31) =>
		       defI31 (x, M.SUB (ity, LI 2, regbind v), e, hp)
		   | _ => error "unexpected numkind in pure ~ primop")
            | gen(PURE(P.copy ft, [v], x, _, e), hp) =
               (case ft
                of (31,32) =>
		     defI32(x, M.SRL(ity, regbind v, one), e, hp)
                 | (8,31) =>
		     copy(I31, x, v, e, hp)
                 | (8,32) =>
		     defI32(x, M.SRL(ity, regbind v, one), e, hp)
                 | (n,m) => if n = m then copyM(m, x, v, e, hp)
				       else error "gen:PURE:copy"
               (*esac*))
	    | gen (PURE(P.copy_inf _, _, _, _, _), hp) =
	        error "gen:PURE:copy_inf"
            | gen(PURE(P.extend ft, [v], x, _ ,e), hp) =
              (case ft
               of (8,31) =>
                    defI31(x,
                       M.SRA(ity, M.SLL(ity, regbind v,LI 23), LI 23),
                          e, hp)
                | (8,32) =>
                    defI32(x,
                       M.SRA(ity, M.SLL(ity, regbind v, LI 23), LI 24),
                          e, hp)
                | (31,32) =>
		    defI32(x, M.SRA(ity, regbind v, one), e, hp)
                | (n,m) => if n = m then copyM(m, x, v, e, hp)
				      else error "gen:PURE:extend"
                (*esac*))
	    | gen (PURE(P.extend_inf _, _, _, _, _), hp) =
	        error "gen:PURE:extend_inf"
            | gen(PURE(P.trunc ft, [v], x, _, e), hp) =
              (case ft
               of (32,31) =>
                   defI31(x, M.ORB(ity, M.SLL(ity, regbind v, one), one), e, hp)
                | (31,8) =>
		    defI32(x, M.ANDB(ity, regbind v, LI 0x1ff), e, hp)
                | (32,8) =>
		    defI32(x, tagUnsigned(M.ANDB(ity, regbind v,
                                          LI 0xff)), e, hp)
                | (n,m) => if n = m then copyM(m, x, v, e, hp)
				      else error "gen:PURE:trunc"
               (*esac*))
	    | gen (PURE(P.trunc_inf _, _, _, _, _), hp) =
	        error "gen:PURE:trunc_inf"
            | gen(PURE(P.objlength, [v], x, _, e), hp) =
                defI31(x, orTag(getObjLength(v)), e, hp)
            | gen(PURE(P.length, [v], x, t, e), hp) = select(1, v, x, t, e, hp)
            | gen(PURE(P.subscriptv, [v, INT i], x, t, e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  defBoxed(x, M.LOAD(ity, scaleWord(a, INT i), mem'), e, hp)
              end
            | gen(PURE(P.subscriptv, [v, w], x, _, e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  defBoxed(x, M.LOAD(ity, scaleWord(a, w), mem'), e, hp)
              end
            | gen(PURE(P.pure_numsubscript{kind=P.INT 8}, [v,i], x, _, e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in defI31(x,tagUnsigned(M.LOAD(8,scale1(a, i), mem')), e, hp)
              end
            | gen(PURE(P.gettag, [v], x, _, e), hp) =
                defI31(x, tagUnsigned(M.ANDB(ity,
                             getObjDescriptor(v), LI(D.powTagWidth-1))),
                      e, hp)
            | gen(PURE(P.mkspecial, [i, v], x, _, e), hp) =
              let val desc = case i
                  of INT n => LI(dtoi(D.makeDesc(n, D.tag_special)))
                   | _ => M.ORB(ity, M.SLL(ity, untagSigned(i),LI D.tagWidth),
                                LI(dtoi D.desc_special))
              in  (* What gc types are the components? *)
                  treeifyAlloc(x,
                    allocRecord(markNothing, memDisambig x,
                                desc, [(v, offp0)], hp),
                    e, hp+8)
              end
            | gen(PURE(P.makeref, [v], x, _, e), hp) =
              let val tag = LI(dtoi D.desc_ref)
                  val mem = memDisambig x
              in  emit(M.STORE(ity,M.ADD(addrTy,C.allocptr,LI hp),tag,mem));
                  emit(M.STORE(ity,M.ADD(addrTy,C.allocptr,LI(hp+ws)),
                               regbind' v, mem));
                  treeifyAlloc(x, hp+ws, e, hp+2*ws)
              end
            | gen(PURE(P.fwrap,[u],w,_,e), hp) = mkFblock([(u, offp0)],w,e,hp)
            | gen(PURE(P.funwrap,[u],w,_,e), hp) = fselect(0,u,w,e,hp)
            | gen(PURE(P.iwrap,[u],w,_,e), _) = error "iwrap not implemented"
            | gen(PURE(P.iunwrap,[u],w,_,e), _) = error "iunwrap not implemented"
            | gen(PURE(P.i32wrap,[u],w,_,e), hp) =
                mkI32block([(u, offp0)], w, e, hp)
            | gen(PURE(P.i32unwrap,[u],w,_,e), hp) =
                select(0, u, w, INT32t, e, hp)

            | gen(PURE(P.wrap,[u],w,_,e), hp) = copy(PTR, w, u, e, hp)
            | gen(PURE(P.unwrap,[u],w,_,e), hp) = copy(I32, w, u, e, hp)

                (* Note: the gc type is unsafe! XXX *)
            | gen(PURE(P.cast,[u],w,_,e), hp) = copy(PTR, w, u, e, hp)

            | gen(PURE(P.getcon,[u],w,t,e), hp) = select(0,u,w,t,e,hp)
            | gen(PURE(P.getexn,[u],w,t,e), hp) = select(0,u,w,t,e,hp)
            | gen(PURE(P.getseqdata, [u], x, t, e), hp) = select(0,u,x,t,e,hp)
            | gen(PURE(P.recsubscript, [v, INT w], x, t, e), hp) =
                select(w,v,x,t,e,hp)
            | gen(PURE(P.recsubscript, [v, w], x, _, e), hp) =
                 (* no indirection! *)
              let val mem = arrayRegion(getRegion v)
              in  defI31(x, M.LOAD(ity, scaleWord(regbind v, w), mem), e, hp)
              end
            | gen(PURE(P.raw64subscript, [v, i], x, _, e), hp) =
              let val mem = arrayRegion(getRegion v)
              in  treeifyDefF64(x, M.FLOAD(fty,scale8(regbind v, i), mem),
                                e, hp)
              end
            | gen(PURE(P.newarray0, [_], x, t, e), hp) =
              let val hdrDesc = dtoi(D.desc_polyarr)
                  val dataDesc = dtoi D.desc_ref
                  val dataPtr = newReg PTR
                  val hdrM = memDisambig x
                  val (tagM, valM) = (hdrM, hdrM) (* Allen *)
              in  (* gen code to allocate "ref()" for array data *)
                  emit(M.STORE(ity, M.ADD(addrTy, C.allocptr, LI hp),
                               LI dataDesc, tagM));
                  emit(M.STORE(ity, M.ADD(addrTy, C.allocptr, LI(hp+ws)),
                               mlZero, valM));
                  emit(M.MV(pty, dataPtr, M.ADD(addrTy,C.allocptr,LI(hp+ws))));
                  (* gen code to allocate array header *)
                  treeifyAlloc(x,
                     allocHeaderPair(hdrDesc, hdrM, dataPtr, 0, hp+2*ws),
                        e, hp+5*ws)
              end
            | gen(PURE(P.rawrecord NONE, [INT n], x, _, e), hp) =
                (* allocate space for CPS spilling *)
                treeifyAlloc(x, hp, e, hp+n*ws) (* no tag! *)
            | gen(PURE(P.rawrecord (SOME rk), [INT n], x, _, e), hp) =
                (* allocate an uninitialized record with a tag *)
              let val (tag, fp) = (* tagged version *)
                    case rk of
                      (RK_FCONT | RK_FBLOCK) => (D.tag_raw64, true)
                    | RK_I32BLOCK => (D.tag_raw32, false)
                    | RK_VECTOR => error "rawrecord VECTOR unsupported"
                    | _ => (D.tag_record, false)

                  (* len of record in 32-bit words *)
                  val len = if ws = 4 andalso fp then n+n else n

                  (* record descriptor *)
                  val desc = dtoi(D.makeDesc(len, tag))

                  (* Align floating point *)
                  val hp = if ws = 4 andalso fp andalso
                    Word.andb(Word.fromInt hp, 0w4) <> 0w0 then hp+4 else hp

                  val mem = memDisambig x
              in  (* store tag now! *)
                  emit(M.STORE(ity, ea(C.allocptr, hp), LI desc, pi(mem, ~1)));

                  (* assign the address to x *)
                  treeifyAlloc(x, hp+ws, e, hp+len*ws+ws)
              end

            | gen(PURE(P.condmove cmp, vw, x, t, e), hp) =
                condmove(cmp, vw, x, t, e, hp)

            (*** ARITH ***)
            | gen(ARITH(P.arith{kind=P.INT 31, oper=P.~}, [v], x, _, e), hp) =
                (updtHeapPtr hp;
                 defI31(x, M.SUBT(ity, LI 2, regbind v), e, 0)
                )
            | gen(ARITH(P.arith{kind=P.INT 31, oper}, [v, w], x, _, e), hp) =
              (updtHeapPtr hp;
               let val t =
                   case oper
                    of P.+ => int31add(M.ADDT, v, w)
                     | P.- => int31sub(M.SUBT, v, w)
                     | P.* => int31mul(true, M.MULT, v, w)
                     | P./ => int31div(true, M.DIV_TO_ZERO, v, w)
		     | P.div => int31div(true, M.DIV_TO_NEGINF, v, w)
		     | P.rem => int31rem(true, M.DIV_TO_ZERO, v, w)
		     | P.mod => int31rem(true, M.DIV_TO_NEGINF, v, w)
                     | _   => error "gen:ARITH INT 31"
               in  defI31(x, t, e, 0) end
              (*esac*))
            | gen(ARITH(P.arith{kind=P.INT 32, oper}, [v,w], x, _, e), hp) =
              (updtHeapPtr hp;
               case oper
                of P.+   => arith32(M.ADDT, v, w, x, e, 0)
                 | P.-   => arith32(M.SUBT, v, w, x, e, 0)
                 | P.*   => arith32(M.MULT, v, w, x, e, 0)
                 | P./   => arith32(fn(ty,x,y)=>M.DIVT(M.DIV_TO_ZERO,ty,x,y),
				    v, w, x, e, 0)
		 | P.div => arith32(fn(ty,x,y)=>M.DIVT(M.DIV_TO_NEGINF,ty,x,y),
				    v, w, x, e, 0)
		 | P.rem => arith32(fn(ty,x,y)=>M.REMS(M.DIV_TO_ZERO,ty,x,y),
				    v, w, x, e, 0)
		 | P.mod => arith32(fn(ty,x,y)=>M.REMS(M.DIV_TO_NEGINF,ty,x,y),
				    v, w, x, e, 0)
                 | _ => error "P.arith{kind=INT 32, oper}, [v,w], ..."
              (*esac*))
            | gen(ARITH(P.arith{kind=P.INT 32, oper=P.~ }, [v], x, _, e), hp) =
                (updtHeapPtr hp;
                 defI32(x, M.SUBT(ity, zero, regbind v), e, 0))

              (* Note: for testu operations we use a somewhat arcane method
               * to generate traps on overflow conditions. A better approach
               * would be to generate a trap-if-negative instruction available
               * on a variety of machines, e.g. mips and sparc (maybe others).
               *)
            | gen(ARITH(P.testu(32,32), [v], x, _, e), hp) =
              let val xreg = newReg I32
                  val vreg = regbind v
              in  updtHeapPtr hp;
                  emit(M.MV(ity, xreg, M.ADDT(ity, vreg,
                                              regbind(INT32 0wx80000000))));
                  defI32(x, vreg, e, 0)
              end
            | gen(ARITH(P.testu(31,31), [v], x, _, e), hp) =
              let val xreg = newReg I31
                  val vreg = regbind v
              in  updtHeapPtr hp;
                  emit(M.MV(ity,xreg,M.ADDT(ity, vreg,
                                            regbind(INT32 0wx80000000))));
                  defI31(x, vreg, e, 0)
              end
            | gen(ARITH(P.testu(32,31), [v], x, _, e), hp) =
              let val vreg = regbind v
                  val tmp = newReg I32
                  val tmpR = M.REG(ity,tmp)
                  val lab = newLabel ()
              in  emit(M.MV(ity, tmp, regbind(INT32 0wx3fffffff)));
                  updtHeapPtr hp;
                  emit
		    (branchWithProb(M.BCC(M.CMP(32, M.LEU, vreg, tmpR),lab),
				    SOME Probability.likely));
                  emit(M.MV(ity, tmp, M.SLL(ity, tmpR, one)));
                  emit(M.MV(ity, tmp, M.ADDT(ity, tmpR, tmpR)));
                  defineLabel lab;
                  defI31(x, tagUnsigned(vreg), e, 0)
              end
	    | gen (ARITH(P.testu _, _, _, _, _), hp) =
	        error "gen:ARITH:testu with unexpected precisions (not implemented)"
            | gen(ARITH(P.test(32,31), [v], x, _, e), hp) =
               (updtHeapPtr hp; defI31(x, tagSigned(regbind v), e, 0))
            | gen(ARITH(P.test(n, m), [v], x, _, e), hp) =
               if n = m then copyM(m, x, v, e, hp) else error "gen:ARITH:test"
	    | gen (ARITH(P.test_inf _, _, _, _, _), hp) =
	        error "gen:ARITH:test_inf"
            | gen(ARITH(P.arith{oper, kind=P.FLOAT 64}, [v,w], x, _, e), hp) =
              let val v = fregbind v
                  val w = fregbind w
                  val t =
                  case oper
                    of P.+ => M.FADD(fty, v, w)
                     | P.* => M.FMUL(fty, v, w)
                     | P.- => M.FSUB(fty, v, w)
                     | P./ => M.FDIV(fty, v, w)
		     | _ => error "unexpected primop in binary float64"
              in  treeifyDefF64(x, t, e, hp)
              end
            (*** LOOKER ***)
            | gen(LOOKER(P.!, [v], x, _, e), hp) =
              let val mem = arrayRegion(getRegion v)
              in  defBoxed (x, M.LOAD(ity, regbind v, mem), e, hp)
              end
            | gen(LOOKER(P.subscript, [v,w], x, _, e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  defBoxed (x, M.LOAD(ity, scaleWord(a, w), mem'), e, hp)
              end
            | gen(LOOKER(P.numsubscript{kind=P.INT 8},[v,i],x,_,e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  defI31(x, tagUnsigned(M.LOAD(8,scale1(a, i), mem')), e, hp)
              end
            | gen(LOOKER(P.numsubscript{kind=P.FLOAT 64}, [v,i], x, _, e), hp)=
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  treeifyDefF64(x, M.FLOAD(fty,scale8(a, i), mem'), e, hp)
              end
            | gen(LOOKER(P.gethdlr,[],x,_,e), hp) = defBoxed(x, C.exnptr(vfp), e, hp)
            | gen(LOOKER(P.getvar, [], x, _, e), hp) = defBoxed(x, C.varptr(vfp), e, hp)
            | gen(LOOKER(P.getspecial, [v], x, _, e), hp) =
                defBoxed(x, orTag(M.SRA(ity, getObjDescriptor(v),
                                             LI (D.tagWidth-1))),
                         e, hp)
            | gen(LOOKER(P.getpseudo, [i], x, _, e), hp) =
                (print "getpseudo not implemented\n"; nop(x, i, e, hp))
            | gen(LOOKER(P.rawload { kind }, [i], x, _, e), hp) =
                rawload (kind, regbind i, x, e, hp)
            | gen(LOOKER(P.rawload { kind }, [i,j], x, _, e), hp) =
                rawload (kind, M.ADD(addrTy,regbind i, regbind j), x, e, hp)

            (*** SETTER ***)
            | gen(SETTER(P.rawupdate FLTt,[v,i,w],e),hp) =
                (emit(M.FSTORE(fty,scale8(regbind' v, i), fregbind w,R.memory));
                 gen(e, hp))
            | gen(SETTER(P.rawupdate _,[v,i,w],e),hp) =
                  (* XXX Assumes 32-bit. Needs 64-bit support later! *)
                (emit(M.STORE(ity,scaleWord(regbind' v, i), regbind' w,R.memory));
                 gen(e, hp))

            | gen(SETTER(P.assign, [a as VAR arr, v], e), hp) =
              let val ea = regbind a
                  val mem = arrayRegion(getRegion a)
              in  recordStore(ea, hp);
                  emit(M.STORE(ity, ea, regbind v, mem));
                  gen(e, hp+2*ws)
              end
            | gen(SETTER(P.unboxedassign, [a, v], e), hp) =
              let val mem = arrayRegion(getRegion a)
              in  emit(M.STORE(ity, regbind a, regbind v, mem));
                  gen(e, hp)
              end
            | gen(SETTER(P.update, [v,i,w], e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val tmpR = Cells.newReg() (* derived pointer! *)
                  val tmp  = M.REG(ity, tmpR)
                  val ea   = scaleWord(a, i)  (* address of updated cell *)
                  val mem' = arrayRegion(mem)
              in  emit(M.MV(ity, tmpR, ea));
                  recordStore(tmp, hp);
                  emit(M.STORE(ity, tmp, regbind w, mem'));
                  gen(e, hp+2*ws)
              end
            | gen(SETTER(P.unboxedupdate, [v, i, w], e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  emit(M.STORE(ity, scaleWord(a, i), regbind w, mem'));
                  gen(e, hp)
              end
            | gen(SETTER(P.numupdate{kind=P.INT 8}, [s,i,v], e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind s, mem))
                  val ea   = scale1(a, i)
                  val mem' = arrayRegion mem
              in  emit(M.STORE(8, ea, untagUnsigned(v), mem'));
                  gen(e, hp)
              end
            | gen(SETTER(P.numupdate{kind=P.FLOAT 64},[v,i,w],e), hp) =
              let (* get data pointer *)
                  val mem  = dataptrRegion v
                  val a    = markPTR(M.LOAD(ity, regbind v, mem))
                  val mem' = arrayRegion mem
              in  emit(M.FSTORE(fty,scale8(a, i), fregbind w, mem'));
                  gen(e, hp)
              end
            | gen(SETTER(P.setspecial, [v, i], e), hp) =
              let val ea = M.SUB(ity, regbind v, LI 4)
                  val i' =
                    case i
                      of INT k => LI(dtoi(D.makeDesc(k, D.tag_special)))
                     | _ => M.ORB(ity, M.SLL(ity, untagSigned(i),
                                             LI D.tagWidth),
                                  LI(dtoi D.desc_special))
                  val mem = getRegionPi(v, 0)
              in  emit(M.STORE(ity, ea, i', mem));
                  gen(e, hp)
              end
            | gen(SETTER(P.sethdlr,[x],e), hp) =
                (emit(assign(C.exnptr(vfp), regbind x)); gen(e, hp))
            | gen(SETTER(P.setvar,[x],e), hp) =
                (emit(assign(C.varptr(vfp), regbind x)); gen(e, hp))
            | gen(SETTER(P.acclink,_,e), hp) = gen(e, hp)
            | gen(SETTER(P.setmark,_,e), hp) = gen(e, hp)
            | gen(SETTER(P.free,[x],e), hp) = gen(e, hp)
            | gen(SETTER(P.setpseudo,_,e), hp) =
                (print "setpseudo not implemented\n"; gen(e, hp))
            | gen (SETTER (P.rawstore { kind }, [i, x], e), hp) =
                (rawstore (kind, regbind i, x); gen (e, hp))
            | gen (SETTER (P.rawstore { kind }, [i, j, x], e), hp) =
                (rawstore (kind, M.ADD(addrTy, regbind i, regbind j), x);
                 gen (e, hp))
	    | gen (RCC(arg as (_, _, _, _, wtl, e)), hp) =
              let val {result, hp} =
                      CPSCCalls.c_call
                          {stream=stream, regbind=regbind,
                           fregbind=fregbind, typmap=typmap, vfp=vfp, hp=hp}
                          arg
              in  case (result, wtl) of
                    ([], [(w, _)]) => defI31 (w, mlZero, e, hp)
		  | ([M.FPR x],[(w,CPS.FLTt)]) => treeifyDefF64 (w, x, e, hp)
                        (* more sanity checking here ? *)
                  | ([M.GPR x],[(w,CPS.INT32t)]) => defI32 (w, x, e, hp)
                  | ([M.GPR x],[(w,CPS.PTRt _)]) => defBoxed (w, x, e, hp)
		  | ([M.GPR x1, M.GPR x2],[(w1,CPS.INT32t),(w2,CPS.INT32t)]) =>
		      let val (r1, r2) = (newReg I32, newReg I32)
		      in addRegBinding(w1, r1);
			 addRegBinding(w2, r2);
			 emit(M.MV(ity,r1,x1));
			 emit(M.MV(ity,r2,x2));
			 gen(e,hp)
		      end
		  | _ => error "RCC: bad results"
	      end

            (*** BRANCH  ***)
            | gen(BRANCH(P.cmp{oper,kind=P.INT 31},[INT v, INT k],_,e,d), hp) =
              if (case oper
                    of P.> => v>k
                     | P.>= => v>=k
                     | P.< => v<k
                     | P.<= => v<=k
                     | P.eql => v=k
                     | P.neq => v<>k
                (*esac*))
              then gen(e, hp)
              else gen(d, hp)
            | gen(BRANCH(P.cmp{oper,kind=P.INT 32},[INT32 v',INT32 k'],_,e,d), hp) = let
		val v = Word32.toLargeIntX v'
		val k = Word32.toLargeIntX k'
		in
		  if (case oper
                       of P.> => v>k
                	| P.>= => v>=k
                	| P.< => v<k
                	| P.<= => v<=k
                	| P.eql => v=k
                	| P.neq => v<>k
		      (* end case *))
		    then gen(e, hp)
		    else gen(d, hp)
		end
            | gen(BRANCH(P.cmp{oper, kind=P.INT 31}, vw, p, e, d), hp) =
                branch(p, signedCmp oper, vw, e, d, hp)
            | gen(BRANCH(P.cmp{oper,kind=P.UINT 31},[INT v', INT k'],_,e,d),hp)=
              let open Word
                  val v = fromInt v'
                  val k = fromInt k'
              in  if (case oper
                        of P.> => v>k
                         | P.>= => v>=k
                         | P.< => v<k
                         | P.<= => v<=k
                         | P.eql => v=k
                         | P.neq => v<>k
                    (*esac*))
                  then gen(e, hp)
                  else gen(d, hp)
              end
            | gen(BRANCH(P.cmp{oper, kind=P.UINT 31}, vw, p, e, d), hp) =
                branch(p, unsignedCmp oper, vw, e, d, hp)
            | gen(BRANCH(P.cmp{oper,kind=P.UINT 32},[INT32 v,INT32 k],_,e,d),
                  hp) =
              let open Word32
              in  if (case oper
                        of P.> => v>k
                         | P.>= => v>=k
                         | P.< => v<k
                         | P.<= => v<=k
                         | P.eql => v=k
                         | P.neq => v<>k
                    (*esac*))
                  then gen(e, hp)
                  else gen(d, hp)
              end
            | gen(BRANCH(P.cmp{oper, kind=P.UINT 32}, vw, p, e, d), hp) =
                branch(p, unsignedCmp oper, vw, e, d, hp)

            | gen(BRANCH(P.cmp{oper, kind=P.INT 32}, vw, p, e, d), hp) =
                branch(p, signedCmp oper, vw, e, d, hp)
	    | gen(BRANCH(P.fcmp{oper=P.fsgn,size=64}, [v], p, d, e), hp) = let
	        val trueLab = newLabel ()
	        val r = fregbind v
                val r' = newReg I32
                val rReg = M.REG(ity, r')
              (* address of the word that contains the sign bit *)
                val addr = if MachineSpec.bigEndian
                      then M.ADD(addrTy, C.allocptr, LI hp)
                      else M.ADD(pty, rReg, LI((fty - pty) div 8))
                in
                  emit(M.MV(ity, r', M.ADD(addrTy, C.allocptr, LI hp)));
	      	  emit(M.FSTORE(fty,rReg,r,R.memory));
                  emit(M.BCC(M.CMP(ity, M.LT, M.LOAD(ity, addr, R.memory), zero), trueLab));
		  genCont(e, hp);
		  genlab(trueLab, d, hp)
                end
            | gen(BRANCH(P.fcmp{oper,size=64}, [v,w], p, d, e), hp) =
              let val trueLab = newLabel ()
                  val cmp     = real64Cmp(oper, v, w)
              in  emit(M.BCC(cmp, trueLab));
                  genCont(e, hp);
                  genlab(trueLab, d, hp)
              end
            | gen(BRANCH(P.peql, vw, p, e,d), hp) = branch(p, M.EQ, vw, e, d, hp)
            | gen(BRANCH(P.pneq, vw, p, e, d), hp) = branch(p, M.NE, vw, e, d, hp)
            | gen(BRANCH(P.strneq, [INT n,v,w], p, d, e), hp) =
                branchStreq(n,v,w,e,d,hp)
            | gen(BRANCH(P.streq, [INT n,v,w],p,d,e), hp) =
                branchStreq(n,v,w,d,e,hp)
            | gen(BRANCH(P.boxed, [x], p, a, b), hp) = branchOnBoxed(p,x,a,b,hp)
            | gen(BRANCH(P.unboxed, [x], p, a, b), hp) = branchOnBoxed(p,x,b,a,hp)
            | gen(e, hp) =  (PPCps.prcps e; print "\n"; error "genCluster.gen")

         end (*local*)

          fun fragComp() =
          let fun continue() = fcomp (Frag.next())
              and fcomp(NONE) = ()
                | fcomp(SOME(_, Frag.KNOWNFUN _)) = continue()
                | fcomp(SOME(_, Frag.KNOWNCHK _)) = continue()
                | fcomp(SOME(_, Frag.STANDARD{func=ref NONE, ...})) = continue()
                | fcomp(SOME(lab,
                        Frag.STANDARD{func as ref(SOME (zz as (k,f,vl,tl,e))),
                                              ...})) =
                  let val formals = ArgP.standard{fnTy=typmap f, argTys=tl, vfp=vfp}
                  in  func := NONE;
		      pseudoOp(PB.ALIGN_SZ 2);
                      genCPSFunction(lab, k, f, vl, formals, tl, e);
                      continue()
                  end
          in  fcomp (Frag.next())
          end (* fragComp *)

          (*
           * execution starts at the first CPS function -- the frag
           * is maintained as a queue.
           *)
          fun initFrags (start::rest : CPS.function list) =
              let fun init(func as (fk, f, _, _, _)) =
                      addGenTbl (f, Frag.makeFrag(func, functionLabel f))
              in
		  app init rest;
		  init start
              end
	    | initFrags [] = error "initFrags"

          (*
           * Create cluster annotations.
           * Currently, we only need to enter the appropriate
           * gc map information.
           *)
          fun clusterAnnotations() = let
	    val cellinfo =
               if gctypes then
                  let fun enter(M.REG(_,r),ty) = enterGC(r, ty)
                        | enter _ = ()
                  in  enterGC(allocptrR, SMLGCType.ALLOCPTR);
                      enter(C.limitptr(vfp), SMLGCType.LIMITPTR);
                      enter(C.baseptr(vfp), PTR);
                      enter(C.stdlink(vfp), PTR);
                      [#create An.PRINT_CELLINFO(GCCells.printType)
                       ]
                  end
               else []
	   in
	     if vfp then #set An.USES_VIRTUAL_FRAME_POINTER ((), cellinfo)
	     else cellinfo
	   end
      in
	initFrags cluster;
	beginCluster 0;
	pseudoOp PB.TEXT;
	fragComp();
	InvokeGC.emitLongJumpsToGCInvocation stream;
	compile(endCluster(clusterAnnotations()))
      end (* genCluster *)

      fun finishCompilationUnit file = let
	val stream = MLTreeComp.selectInstructions (Flowgen.build ())
	val TS.S.STREAM{beginCluster, pseudoOp, endCluster, ...} = stream
      in
	Cells.reset();
	ClusterAnnotation.useVfp := false;
	beginCluster 0;
	pseudoOp PB.TEXT;
	InvokeGC.emitModuleGC stream;
	pseudoOp (PB.DATA_READ_ONLY);
	pseudoOp (PB.EXT(CPs.FILENAME file));
	compile(endCluster NO_OPT)
      end

      fun entrypoint ((_,f,_,_,_)::_) () = Label.addrOf (functionLabel f)
	| entrypoint [] () = error "entrypoint: no functions"
  in
    app mkGlobalTables funcs;
    app genCluster (Cluster.cluster funcs);
    finishCompilationUnit source;
    entrypoint (funcs)
  end (* codegen *)
end (* MLRiscGen *)

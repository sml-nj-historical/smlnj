(*
 * This module is responsible for generating code to invoke the 
 * garbage collector.  This new version is derived from the functor CallGC.
 * It can handle derived pointers as roots and it can also be used as 
 * callbacks.  These extra facilities are neccessary for global optimizations  
 * in the presence of GC.  (Also, I am afraid of changing the old version
 * since I'm not sure I understand every little detail in it.)
 * 
 * -- Allen
 * 
 *)

functor InvokeGC
   (structure Cells : CELLS
    structure C : CPSREGS where T.Region=CPSRegions
    structure MS: MACH_SPEC
   ) : INVOKE_GC =
struct

   structure T  = C.T
   structure D  = MS.ObjDesc
   structure LE = LabelExp
   structure R  = CPSRegions
   structure S  = SortedList
   structure St = T.Stream
   structure GC = SMLGCType
   structure Cells = Cells

   fun error msg = ErrorMsg.impossible("InvokeGC."^msg)

   type t = { maxAlloc : int,
              regfmls  : T.mlrisc list,
              regtys   : CPS.cty list,
              return   : T.stm
            }

   type stream = (T.stm,Cells.regmap) T.stream

   val debug = Control.MLRISC.getFlag "debug-gc";

   val addrTy = C.addressWidth

   (* GcInfo encapsulates all the information needed to generate
    * code to invoke gc 
    *)
   datatype gcInfo =
      GCINFO of
        {known     : bool,             (* known function ? *)
         optimized : bool,             (* optimized? *)
         lab       : Label.label ref,  (* labels to invoke GC *)
         boxed     : T.rexp list,      (* locations with boxed objects *)
         int32     : T.rexp list,      (* locations with int32 objects *)
         float     : T.fexp list,      (* locations with float objects *)
         regfmls   : T.mlrisc list,    (* all live registers *)
         ret       : T.stm}            (* how to return *)
    | MODULE of
        {info: gcInfo,
         addrs: Label.label list ref} (* addrs associated with long jump *)

   (*====================================================================
    * Implementation/architecture specific stuff starts here.
    *====================================================================*)

      (* Extra space in allocation space 
       * The SML/NJ runtime system leaves around 4K of extra space
       * in the allocation space for safety.
       *)
   val skidPad = 4096
   val pty  = 32

   val sp   = Cells.stackptrR  (* stack pointer *)
   val spR  = T.REG(32,sp)
   val unit = T.LI 1          (* representation of ML's unit; 
                               * this is used to initialize registers
                               *)
       (* callee-save registers 
        *
        *)
   val calleesaves = List.take(C.miscregs, MS.numCalleeSaves)

       (* 
        * registers that are the roots of gc.
        *)
   val gcParamRegs  = (C.stdlink::C.stdclos::C.stdcont::C.stdarg::calleesaves)

       (*
        * How to call the call the GC 
        *)
   local val use = map T.GPR gcParamRegs 
         val def = case C.exhausted of NONE => use 
                                     | SOME cc => T.CCR cc::use
   in  val gcCall = 
          T.ANNOTATION(
          T.CALL(
            T.LOAD(32, T.ADD(addrTy,C.stackptr,T.LI MS.startgcOffset), R.stack),
                   def, use, R.stack),
          #create MLRiscAnnotations.COMMENT "call gc")
   end

   val CALLGC = #create MLRiscAnnotations.CALLGC ()

       (*
        * record descriptors
        *)
   val dtoi = LargeWord.toInt
   fun unboxedDesc words = dtoi(D.makeDesc(words, D.tag_raw64))
   fun boxedDesc words   = dtoi(D.makeDesc(words, D.tag_record))

       (* the allocation pointer must always in a register! *)
   val T.REG(_,allocptrR) = C.allocptr

       (* what type of comparison to use for GC test? *)
   val gcCmp = if C.signedGCTest then T.GT else T.GTU

   val unlikely = #create MLRiscAnnotations.BRANCH_PROB 0

   val normalTestLimit = T.CMP(pty, gcCmp, C.allocptr, C.limitptr)

   (*====================================================================
    * Private state
    *====================================================================*)
   (* gc info required for standard functions within the cluster *)
   val clusterGcBlocks = ref([]: gcInfo list)

   (* gc info required for known functions within the cluster *)
   val knownGcBlocks = ref([]: gcInfo list)

   (* gc info required for modules *)
   val moduleGcBlocks = ref ([]: gcInfo list)

   (*====================================================================
    * Auxiliary functions
    *====================================================================*)

   (*
    * Convert a list of rexps into a set of registers and memory offsets.
    * Memory offsets must be relative to the stack pointer.
    *)
   fun set bindings = 
   let fun isStackPtr sp = sp = Cells.stackptrR
       fun live(T.REG(_,r)::es, regs, mem) = live(es, r::regs, mem)
         | live(T.LOAD(_, T.REG(_, sp), _)::es, regs, mem) =
           if isStackPtr sp then live(es, regs, 0::mem)
           else error "set:LOAD32"
         | live(T.LOAD(_, T.ADD(_, T.REG(_, sp), T.LI i), _)::es, regs, mem) =
           if isStackPtr sp then live(es, regs, i::mem)
           else error "set:LOAD32"
         | live([], regs, mem) = (regs, mem)
         | live _ = error "live"
       val (regs, mem) = live(bindings, [], [])
   in  {regs=S.uniq regs, mem=S.uniq mem} end

   fun difference({regs=r1,mem=m1}, {regs=r2,mem=m2}) =
       {regs=S.difference(r1,r2), mem=S.difference(m1,m2)}
 
   fun setToString{regs,mem} =
       "{"^foldr (fn (r,s) => Cells.toString Cells.GP r^" "^s) "" regs
          ^foldr (fn (m,s) => Int.toString m^" "^s) "" mem^"}"

   fun setToMLTree{regs,mem} =
       map (fn r => T.REG(32,r)) regs @ 
       map (fn i => T.LOAD(32, T.ADD(addrTy, spR, T.LI i), R.memory)) mem
            
   (* The client communicate root pointers to the gc via the following set
    * of registers and memory locations.
    *)
   val gcrootSet = set gcParamRegs 
   val aRoot     = hd(#regs gcrootSet)
   val aRootReg  = T.REG(32,aRoot)

   (* 
    * This function generates a gc limit check.
    * It returns the label to the GC invocation block.
    *) 
   fun checkLimit(emit, maxAlloc) =
   let val lab = Label.newLabel ""
       fun gotoGC(cc) = emit(T.ANNOTATION(T.BCC(gcCmp, cc, lab), unlikely))
   in  if maxAlloc < skidPad then
          (case C.exhausted of
             SOME cc => gotoGC cc
           | NONE => gotoGC(normalTestLimit)
          )
       else  
       let val shiftedAllocPtr = T.ADD(addrTy,C.allocptr,T.LI(maxAlloc-skidPad))
           val shiftedTestLimit = T.CMP(pty, gcCmp, shiftedAllocPtr, C.limitptr)
       in  case C.exhausted of
             SOME(cc as T.CC r) => 
               (emit(T.CCMV(r, shiftedTestLimit)); gotoGC(cc))
           | NONE => gotoGC(shiftedTestLimit)
           | _ => error "checkLimit"
       end;
       lab
   end

   (* 
    * This function recomputes the base pointer address
    *)
   fun computeBasePtr(emit,defineLabel) =
   let val returnLab = Label.newLabel ""
       val baseExp = 
           T.ADD(addrTy, C.gcLink,
                 T.LABEL(LE.MINUS(LE.CONST MS.constBaseRegOffset,
                                  LE.LABEL returnLab)))
   in  defineLabel returnLab;
       emit(case C.baseptr of 
              T.REG(ty, bpt) => T.MV(ty, bpt, baseExp)
            | T.LOAD(ty, ea, mem) => T.STORE(ty, ea, baseExp, mem)
            | _ => error "computeBasePtr")
   end 


   (*
    * Functions to pack and unpack roots. 
    *
    * There are two types of records.  One contains boxed objects
    * (ints and pointers) and another containing unboxed objects 
    * (int32s and reals).  
    *
    *) 
   local
       fun allocF(emit, [], offset) = offset
         | allocF(emit, f::fs, offset) =   
           (emit(T.FSTORE(64, T.ADD(32, C.allocptr, T.LI offset), f, R.memory));
            allocF(emit, fs, offset + 8))
       fun allocR(emit, [], offset) = offset
         | allocR(emit, i::is, offset) = 
           (emit(T.STORE(32, T.ADD(32, C.allocptr, T.LI offset), i, R.memory));
            allocR(emit, is, offset + 4))
       fun doNothing _ = ()

       (*
        * Parallel copy dst <- src.  
        * If pad is true then dst can have more
        * elements than src.  The extra elements are padded with unit.
        *)
       fun move(emit, dst, src, pad) =
       let fun copy([],[]) = ()
             | copy(rd,rs) = emit(T.COPY(32,rd,rs))
           fun loop([],[],rd,rs) = ([],rd,rs)
             | loop([],src,rd,rs) = ([],rd,rs)
             | loop(dst,[],rd,rs) = 
                (if pad then dst else 
                 error("missing src "^
                       Int.toString(length dst)^" "^Int.toString(length src)), 
                rd,rs)
             | loop(T.REG(_,r)::dst,T.REG(_,s)::src,rd,rs) = 
                 loop(dst,src,r::rd,s::rs)
             | loop(T.REG(ty,r)::dst,e::src,rd,rs) =
                 (emit(T.MV(ty,r,e)); loop(dst,src,rd,rs))
             | loop(T.LOAD(ty,ea,mem)::dst,e::src,rd,rs) =
                 (emit(T.STORE(ty,ea,e,mem)); loop(dst,src,rd,rs))
             | loop _ = error "loop"
           val (toPad,dst,src) = loop(dst,src,[],[])
       in  copy(dst,src); toPad
       end

       (* Unpack objects from record, usable by both tagged
        * and untagged objects.
        *)
       fun unpack(emit, record, int, float) () = 
       let fun selectR(off) = T.LOAD(32,T.ADD(addrTy,record,T.LI off),R.memory)
           fun selectF(off) = T.FLOAD(64,T.ADD(addrTy,record,T.LI off),R.memory)
           fun doR([], offset) = ()
             | doR(T.REG(t,r)::es, offset) = 
               (emit(T.MV(t,r,selectR(offset))); doR(es, offset+4))
             | doR(T.LOAD(t,ea,mem)::es, offset) = 
               (emit(T.STORE(t,ea,selectR(offset),mem)); doR(es, offset+4))
             | doR _ = error "unpack.doR"
           fun doF([], offset) = offset
             | doF(T.FREG(t,r)::es, offset) = 
               (emit(T.FMV(t,r,selectF(offset))); doF(es, offset+8))
             | doF(T.FLOAD(t,ea,mem)::es, offset) = 
               (emit(T.FSTORE(t,ea,selectF(offset),mem)); doF(es, offset+8))
             | doF _ = error "unpack.doF"
       in  doR(int, doF(float, 0)) 
       end 

       (* Pack int32s + floats together into a raw64 record. 
        * Return the record pointer, and the new heap pointer offset. 
        *)
       fun packUnboxed(emit, int32, float) = 
       let val qwords = length float + (length int32 + 1) div 2
           val desc   = unboxedDesc(qwords + qwords)
           val _      = 
               (* align the allocptr if we have floating point roots *)
               case float of
                 [] => ()
               | _  => emit(T.MV(32, allocptrR, T.ORB(32, C.allocptr, T.LI 4)))
           val _     = emit(T.STORE(pty, C.allocptr, T.LI desc, R.memory))
           val _     = allocR(emit, int32, allocF(emit, float, 4))
           val t     = Cells.newReg()
       in  emit(T.MV(pty, t, T.ADD(addrTy, C.allocptr, T.LI 4)));
           (T.REG(32,t), qwords * 8 + 4)
       end

       (* 
        * Pack tagged objects into a single record. 
        * Return the record pointer, and the new heap pointer offset.
        *)
       fun packBoxed(emit, hp, boxed) = 
       let val words = length boxed
           val desc  = boxedDesc(words)
           val _     = emit(T.STORE(pty, T.ADD(addrTy, C.allocptr, T.LI hp), 
                                    T.LI desc, R.memory))
           val hp'   = allocR(emit, boxed, hp+4)
           val t     = Cells.newReg()
       in  emit(T.MV(pty, t, T.ADD(addrTy, C.allocptr, T.LI(hp+4))));
           (T.REG(32,t), hp')
       end
 
   in  
       (*
        * Initialize the list of roots with unit
        *)
       fun initRoots(emit,roots) = 
           app (fn T.REG(ty,r) => emit(T.MV(ty,r,unit))
                 | T.LOAD(ty,ea,mem) => emit(T.STORE(ty,ea,unit,mem))
                 | _ => error "initRoots") roots

       (*
        * Pack all the roots together into the appropriate records.
        * Invariant: gcRoots must be non-empty.
        *) 
       fun pack(emit, gcRoots, boxed, int32, float) =
       let (* package up the unboxed things first *)
           val (boxedIn,boxedOut,raw,hp,unpackRaw) = 
               case (int32,float) of 
                 ([],[]) => (boxed, boxed, ~1, 0, doNothing)
               | _ => 
                 let val (r, hp) = packUnboxed(emit, int32, float)
                     val r'      = Cells.newReg()
                     val r''     = T.REG(32,r')
                 in  (r::boxed, r''::boxed, r', 
                      hp, unpack(emit, r'', int32, float))
                 end

           val nGcRoots = length gcRoots
           val nBoxed   = length boxedIn

           (* package up the boxed things if necessary *)
           val (rootsIn, rootsOut, cooked, hp, unpackBoxed) = 
               if nBoxed > nGcRoots then
               let fun take(0, l, front) = (rev front, l) 
                     | take(n, x::xs, front) = take(n-1, xs, x::front)
                     | take _ = error "take"
                   val nRoots = nGcRoots - 1
                   val (restIn, extraIn) = take(nRoots,boxedIn,[])
                   val (restOut, extraOut) = take(nRoots,boxedOut,[])
                   val (r, hp) = packBoxed(emit, hp, extraIn) 
                   val r' = Cells.newReg()
                   val r'' = T.REG(32,r')
               in  (r::restIn, r''::restOut, r', 
                    hp, unpack(emit, r'', extraOut, []))
               end
               else (boxedIn, boxedOut, ~1, hp, doNothing)

           fun unpack() =
           let fun get(r,(d as T.REG(_,r'))::dst,s::src,dst',src') =
                   if r = r' then ([d],[s],rev dst'@dst,rev src'@src)
                   else get(r,dst,src,d::dst',s::src')
                 | get(r,d::dst,s::src,dst',src') =
                     get(r,dst,src,d::dst',s::src') 
                 | get(r,dst,src,dst',src') = ([],[],rev dst'@dst,rev src'@src) 
               val (rawDst,rawSrc,rootsOut,gcRoots) = 
                      get(raw,rootsOut,gcRoots,[],[])
               val (cookedDst,cookedSrc,rootsOut,gcRoots) = 
                      get(cooked,rootsOut,gcRoots,[],[])
           in  (* copy the boxed record root to its temporary *)
               move(emit,cookedDst,cookedSrc,false);
               unpackBoxed();
               (* copy the raw record root to its temporary *)
               move(emit,rawDst,rawSrc,false);
               unpackRaw();
               (* copy the rest of the roots back to its original registers *)
               move(emit,rootsOut,gcRoots,false)
           end
                
       in  (* update the allocation pointer *)
           if hp > 0 then 
              emit(T.MV(pty, allocptrR, T.ADD(addrTy, C.allocptr, T.LI hp)))
           else ();
           (move(emit,gcRoots,rootsIn,true), unpack)
       end

   end

   (*====================================================================
    * Main functions
    *====================================================================*)
   fun init() =
       (clusterGcBlocks        := [];
        knownGcBlocks          := [];
        moduleGcBlocks         := []
       )

   (*
    * Partition the root set into types 
    *)
   fun split([], [], boxed, int32, float) = 
         {boxed=boxed, int32=int32, float=float}
     | split(T.GPR r::rl, CPS.INT32t::tl, b, i, f) = split(rl,tl,b,r::i,f)
     | split(T.GPR r::rl, CPS.FLTt::tl, b, i, f) = error "split: T.GPR"
     | split(T.GPR r::rl, _::tl, b, i, f) = split(rl,tl,r::b,i,f)
     | split(T.FPR r::rl, CPS.FLTt::tl, b, i, f) = split(rl,tl,b,i,r::f)
     | split _ = error "split"

   fun genGcInfo (clusterRef,known,optimized) (St.STREAM{emit,...} : stream) 
                 {maxAlloc, regfmls, regtys, return} =
   let (* partition the root set into the appropriate classes *)
       val {boxed, int32, float} = split(regfmls, regtys, [], [], [])

   in  clusterRef := 
          GCINFO{ known    = known,
                  optimized=optimized,
                  lab      = ref (checkLimit(emit,maxAlloc)),
                  boxed    = boxed,
                  int32    = int32,
                  float    = float,
                  regfmls  = regfmls,
                  ret      = return } :: (!clusterRef)
   end

   (* 
    * Check-limit for standard functions, i.e.~functions with 
    * external entries.
    *)
   val stdCheckLimit = genGcInfo (clusterGcBlocks, false, false)

   (*
    * Check-limit for known functions, i.e.~functions with entries from
    * within the same cluster.
    *)
   val knwCheckLimit = genGcInfo (knownGcBlocks, true, false)

   (*
    * Check-limit for optimized, known functions.  
    *)
   val optimizedKnwCheckLimit = genGcInfo(knownGcBlocks, true, true)

   (*
    * The following auxiliary function generates the actual call gc code. 
    * It packages up the roots into the appropriate
    * records, call the GC routine, then unpack the roots from the record.
    *) 
   fun emitCallGC{stream=St.STREAM{emit, annotation, defineLabel, ...}, 
                  known, boxed, int32, float, ret} =
   let (* IMPORTANT NOTE:  
        * If a boxed root happens be in a gc root register, we can remove
        * this root since it will be correctly targetted. 
        *
        * boxedRoots are the boxed roots that we have to move to the appropriate
        * registers.  gcrootSet are the registers that are available
        * for communicating to the collector.
        *)
       val boxedSet   = set boxed
       val boxedRoots = difference(boxedSet,gcrootSet)  (* roots *)
       val gcrootAvail = difference(gcrootSet,boxedSet) (* gcroots available *)

       fun mark(call) =
           if !debug then
              T.ANNOTATION(call,#create MLRiscAnnotations.COMMENT 
                 ("roots="^setToString gcrootAvail^
                  " boxed="^setToString boxedRoots))
           else call
 
       (* convert them back to MLTREE *)
       val boxed   = setToMLTree boxedRoots 
       val gcroots = setToMLTree gcrootAvail

       (* If we have any remaining roots after the above trick, we have to 
        * make sure that gcroots is not empty.
        *)
       val (gcroots, boxed) = 
           case (gcroots, int32, float, boxed) of
             ([], [], [], []) => ([], []) (* it's okay *)
           | ([], _, _, _) => ([aRootReg], aRootReg::boxed) 
           | _  => (gcroots, boxed)

       val (extraRoots,unpack) = pack(emit, gcroots, boxed, int32, float)
   in  initRoots(emit, extraRoots);
       annotation(CALLGC);
       emit(mark(gcCall));
       if known then computeBasePtr(emit,defineLabel) else ();
       unpack();
       emit ret
   end

   (*
    * The following function is responsible for generating only the
    * callGC code.
    *)
   fun callGC stream {regfmls, regtys, ret} =
   let val {boxed, int32, float} = split(regfmls, regtys, [], [], [])
   in  emitCallGC{stream=stream, known=true, 
                  boxed=boxed, int32=int32, float=float, ret=ret}
   end

   (*
    * The following function is responsible for generating actual
    * GC calling code, with entry labels and return information.
    *)
   fun invokeGC(stream as 
                St.STREAM{emit,defineLabel,entryLabel,exitBlock,annotation,...},
                externalEntry) gcInfo = 
   let val {known, optimized, boxed, int32, float, regfmls, ret, lab} =
           case gcInfo of
             GCINFO info => info
           | MODULE{info=GCINFO info,...} => info
           | _ => error "invokeGC:gcInfo"

       val regfmls = if optimized then [] else regfmls

   in  if externalEntry then entryLabel (!lab) else defineLabel (!lab);
       (* When the known block is optimized, no actual code is generated
        * until later.
        *)
       if optimized then (annotation(CALLGC); emit ret)
       else emitCallGC{stream=stream, known=known,
                       boxed=boxed, int32=int32, float=float, ret=ret};
       exitBlock(case C.exhausted of NONE    => regfmls 
                                   | SOME cc => T.CCR cc::regfmls)
   end

   (*
    * The following function checks whether two root set have the
    * same calling convention.
    *)
   fun sameCallingConvention
          (GCINFO{boxed=b1, int32=i1, float=f1, ret=T.JMP(ret1, _), ...},
           GCINFO{boxed=b2, int32=i2, float=f2, ret=T.JMP(ret2, _), ...}) =
   let fun eqEA(T.REG(_, r1), T.REG(_, r2)) = r1 = r2
         | eqEA(T.ADD(_,T.REG(_,r1),T.LI i), T.ADD(_,T.REG(_,r2),T.LI j)) =  
             r1 = r2 andalso i = j
         | eqEA _ = false
       fun eqR(T.REG(_,r1), T.REG(_,r2)) = r1 = r2
         | eqR(T.LOAD(_,ea1,_), T.LOAD(_,ea2,_)) = eqEA(ea1, ea2)
         | eqR _ = false
       fun eqF(T.FREG(_,f1), T.FREG(_,f2)) = f1 = f2
         | eqF(T.FLOAD(_,ea1,_), T.FLOAD(_,ea2,_)) = eqEA(ea1, ea2)
         | eqF _ = false

       fun all predicate = 
       let fun f(a::x,b::y) = predicate(a,b) andalso f(x,y)
             | f([],[]) = true
             | f _ = false
       in  f end

       val eqRexp = all eqR
   in  eqRexp(b1, b2) andalso eqR(ret1,ret2) andalso 
       eqRexp(i1,i2) andalso all eqF(f1,f2)
   end 
     | sameCallingConvention _ = false

   (*
    * The following function is called once at the end of compiling a cluster.
    * Generates long jumps to the end of the module unit for
    * standard functions, and directly invokes GC for known functions.
    * The actual GC invocation code is not generated yet.
    *)
   fun emitLongJumpsToGCInvocation
       (stream as St.STREAM{emit,defineLabel,exitBlock,...}) =
   let (* GC code can be shared if the calling convention is the same 
        * Use linear search to find the gc subroutine.
        *)
       fun find(info as GCINFO{lab as ref l, ...}) =
       let fun search(MODULE{info=info', addrs}::rest) =
               if sameCallingConvention(info, info') then
                  addrs := l :: (!addrs) 
               else search rest
             | search [] = (* no matching convention *)
               let val label = Label.newLabel ""
               in  lab := label;
                   moduleGcBlocks := MODULE{info=info, addrs=ref[l]} ::
                      (!moduleGcBlocks)
               end
       in  search(!moduleGcBlocks) 
       end

       (*
        * Generate a long jump to all external callgc routines 
        *)
       fun longJumps(MODULE{addrs=ref [],...}) = ()
         | longJumps(MODULE{info=GCINFO{lab,boxed,int32,float,...}, addrs}) =
       let val regRoots  = map T.GPR (int32 @ boxed)
           val fregRoots = map T.FPR float
           val liveOut   = regRoots @ fregRoots
           val l         = !lab
       in  app defineLabel (!addrs) before addrs := [];
           emit(T.JMP(T.LABEL(LE.LABEL l),[l]));
           exitBlock liveOut
       end

   in  app find (!clusterGcBlocks) before clusterGcBlocks := [];
       app longJumps (!moduleGcBlocks);
       app (invokeGC(stream,false)) (!knownGcBlocks) before knownGcBlocks := []
   end (* emitLongJumpsToGC *)

   (* 
    * The following function is called to generate module specific 
    * GC invocation code 
    *)
   fun emitModuleGC(stream) =
       app (invokeGC(stream,true)) (!moduleGcBlocks) before moduleGcBlocks := []

end

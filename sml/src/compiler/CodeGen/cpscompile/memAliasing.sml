(* 
 * Perform memory aliasing analysis.
 *
 * The old memory disambiguation module discards aliasing information
 * across CPS function boundaries, which made it not very useful for the
 * optimizations I have in mind.
 *
 * This is an alternative module that (hopefully) does the right thing.
 * The algorithm is inspired by Steensgaard's work on flow insensitive
 * points-to analysis, but has been hacked to deal with target level issues.
 *
 * Some target level issues
 * ------------------------
 * In the source level two CPS allocations cannot be aliased by definition.
 * However, when allocations are translated into target code, they become
 * stores to fixed offsets from the heap pointer.  Two allocation stores 
 * that may write to the same offset are aliased.  Allocation stores that are
 * in disjoint program paths may be assigned the same heap allocation offset.
 * We have to mark these as aliased since we want to allow speculative writes
 * to the allocation space.
 *
 * Representing heap offsets 
 * -------------------------
 *
 * 
 * Language 
 * --------
 * e ::= x <- v.i; k           /* select */
 *    |  x <- v+i; k           /* offset */
 *    |  x <- [v1,...vn]^hp; k /* record allocation at heap pointer hp */
 *    |  x <- !v; k            /* dereference */
 *    |  v1 := v2; k           /* update */
 *    |  f(v1,...,vn)          /* tail call */
 *
 * Since the analysis is flow insensitive, the branch constructs are 
 * irrelevant.
 *
 * -- Allen
 *)

signature MEM_ALIASING = 
sig
   val analyze : CPS.function list -> (CPS.lvar -> CPSRegions.region)
end

functor MemAliasing(Cells : CELLS) : MEM_ALIASING = 
struct
   structure C  = CPS
   structure P  = CPS.P
   structure PT = PointsTo

   fun error msg = MLRiscErrorMsg.error("MemAliasing",msg)

   (* 
    * The following functions advances the heap pointer.
    * These functions are highly dependent on the runtime system and
    * how data structures are represented.
    * IMPORTANT: we are assuming that the new array representation is used.
    *)
   fun recordSize(n,hp)  =  n * 4 + 4 + hp
   fun frecordSize(n,hp) = 
       let val hp = if Word.andb(Word.fromInt hp,0w4) <> 0w0 then hp+8 else hp+4
       in  8*n + hp end
   fun vectorSize(n,hp) = n * 4 + 16 + hp 

   fun allocRecord(C.RK_FBLOCK,vs,hp) = frecordSize(length vs,hp)
     | allocRecord(C.RK_FCONT,vs,hp)  = frecordSize(length vs,hp)
     | allocRecord(C.RK_VECTOR,vs,hp) = vectorSize(length vs,hp)
     | allocRecord(_,vs,hp) = recordSize(length vs,hp)

   val storeListSize = 8
   val array0Size    = 20

   exception NotFound

   val top = CPSRegions.memory

   (*
    * Analyze a set of CPS functions
    *)
   fun analyze(cpsFunctions) = 
   let fun sizeOf(C.RECORD(rk,vs,x,k),hp) = sizeOf(k,allocRecord(rk,vs,hp))
         | sizeOf(C.SELECT(off,v,x,cty,k),hp) = sizeOf(k,hp)
         | sizeOf(C.OFFSET(off,v,x,k),hp) = sizeOf(k,hp)
         | sizeOf(C.APP(f,vs),hp) = hp
         | sizeOf(C.FIX _,hp) = error "sizeOf: FIX"
         | sizeOf(C.SWITCH(v,x,ks),hp) = sizeOfs(ks,hp)
         | sizeOf(C.BRANCH(p,_,x,k1,k2),hp) = 
             Int.max(sizeOf(k1,hp),sizeOf(k2,hp))
         | sizeOf(C.SETTER(P.assign,vs,k),hp) = sizeOf(k,hp+storeListSize)
         | sizeOf(C.SETTER(P.update,vs,k),hp) = sizeOf(k,hp+storeListSize)
         | sizeOf(C.SETTER(P.boxedupdate,vs,k),hp) = sizeOf(k,hp+storeListSize)
         | sizeOf(C.SETTER(_,vs,k),hp) = sizeOf(k,hp)
         | sizeOf(C.PURE(P.fwrap,vs,x,cty,k),hp) = sizeOf(k,frecordSize(1,hp))
         | sizeOf(C.PURE(P.mkspecial,vs,x,cty,k),hp) = sizeOf(k,hp+8)
         | sizeOf(C.PURE(P.makeref,vs,x,cty,k),hp) = sizeOf(k,hp+8)
         | sizeOf(C.PURE(P.i32wrap,vs,x,cty,k),hp) = sizeOf(k,hp+8)
         | sizeOf(C.PURE(P.newarray0,vs,x,cty,k),hp) = sizeOf(k,hp+array0Size)
         | sizeOf(C.PURE(p,vs,x,cty,k),hp) = sizeOf(k,hp)
         | sizeOf(C.ARITH(a,vs,x,cty,k),hp) = sizeOf(k,hp)
         | sizeOf(C.LOOKER(lk,vs,x,cty,k),hp) = sizeOf(k,hp)
 
       and sizeOfs([],hp)    = hp
         | sizeOfs(k::ks,hp) = Int.max(sizeOf(k,hp),sizeOfs(ks,hp))

       val locMap = Intmap.new(37,NotFound) (* lvar -> loc *)
       val look   = Intmap.map locMap
       val bind   = Intmap.add locMap 

       val newMem = Cells.newCell Cells.MEM

       val _      = PT.reset newMem

       fun newRef _ = ref(PT.SCELL(newMem(),ref []))

       val exnptr = PT.newSRef() (* exception handler *)
       val varptr = PT.newSRef() (* var ptr *)

       fun lookup x =
           look x handle _ =>
           let val r = newRef() in bind(x,r); r end

 
       fun defineFunction(fk, f, args, _, cexp) = 
           let val xs = 
                map (fn x => let val r = newRef() in bind(x,r); r end) args
           in  bind(f, PT.mkLambda xs) end 

       val off0 = C.OFFp 0

       fun process(fk, f, args, _, cexp) = 
       let (* create a table of allocation offset locations *)
           val table = Array.tabulate(sizeOf(cexp, 0) div 4, newRef)

           fun select(i,C.VAR v,x) = bind(x,PT.pi(lookup v,i))
             | select(i,_,x)       = ()

           fun offset(i,C.VAR v,x) = bind(x,PT.offset(lookup v,i))
             | offset(i,_,x)       = ()

           fun value (C.VAR v) = lookup v
             | value _         = newRef()

           fun apply(C.VAR f,args) = PT.app(lookup f,map value args)
             | apply _             = ()

           fun getPath(v,C.OFFp 0) = value v
             | getPath(v,C.OFFp n) = PT.offset(value v, n)
             | getPath(v,C.SELp(n,path)) = PT.pi(getPath(v,path),n)

           fun getPaths([],hp) = []
             | getPaths((v,path)::vs,hp) = 
               let val r  = Array.sub(table,hp)
                   val r' = getPath(v,path)
               in  PT.unify(r,r'); r::getPaths(vs,hp+1) end

           fun getF64Paths([],hp) = []
             | getF64Paths((v,path)::vs,hp) = 
               let val r1  = Array.sub(table,hp)
                   val r2  = Array.sub(table,hp+1)
                   val r'  = getPath(v,path)
               in  PT.unify(r1,r'); PT.unify(r2,r');
                   r'::getF64Paths(vs,hp+2) 
               end

           (* How to make a record *)
           fun mkRec(f,getPaths,x,vs,hp) = 
               let val i = Word.toInt(Word.>>(Word.fromInt hp,0w2))
                   val r = f(SOME(Array.sub(table,i)),getPaths(vs,i+1))
               in  bind(x,r) end
           fun mkFRecord(x,vs,hp) = mkRec(PT.mkRecord,getF64Paths,x,vs,hp)
           fun mkVector(x,vs,hp) = mkRec(PT.mkRecord,getPaths,x,vs,hp)
           fun mkNormalRecord(x,vs,hp) = mkRec(PT.mkRecord,getPaths,x,vs,hp)

           fun mkRecord(C.RK_FBLOCK,x,vs,hp) = mkFRecord(x,vs,hp)
             | mkRecord(C.RK_FCONT,x,vs,hp) = mkFRecord(x,vs,hp)
             | mkRecord(C.RK_VECTOR,x,vs,hp) = mkVector(x,vs,hp) 
             | mkRecord(_,x,vs,hp) = mkNormalRecord(x,vs,hp) 

           fun makeTop(m) = (PT.unify(m, top); top)

           (* CPS Pure Primitives *)
           fun arrayptr v = PT.pi(value v, 0)

           fun mkspecial(x,v,hp) = mkNormalRecord(x,[(v,off0)],hp)
           fun fwrap(x,v,hp) = mkFRecord(x,[(v,off0)],hp)
           fun i32wrap(x,v,hp) = mkNormalRecord(x,[(v,off0)],hp)
           fun makeref(x,v,hp) = mkNormalRecord(x,[(v,off0)],hp)
           fun newarray0(x,hp) = 
               bind(x,PT.mkRecord(NONE,[PT.mkRecord(NONE,[])]))

           fun objlength(x,v) = bind(x, PT.pi(value v, ~1))
           fun length(x,v) = bind(x, PT.pi(value v, 1))
           fun arraysub(x,a,i) = makeTop(PT.weakSubscript(arrayptr a))
           fun subscriptv(x,a,i) = arraysub(x,a,i)
           fun subscript(x,a,i) = arraysub(x,a,i)
           fun pure_numsubscript(x,a,i) = arraysub(x,a,i)
           fun gettag(x,v) = bind(x,PT.pi(value v, ~1))
           fun numsubscript8(x,a,i) = arraysub(x,a,i)
           fun numsubscriptf64(x,a,i) = arraysub(x,a,i)
           fun getcon(x,v) = bind(x, PT.pi(value v,0))
           fun getexn(x,v) = bind(x, PT.pi(value v,0))
           fun recsubscript(x,a,i) = arraysub(x,a,i)
           fun raw64subscript(x,a,i) = arraysub(x,a,i)

           (* CPS Looker Primitives *)
           fun deref(x,v) = makeTop(PT.strongSubscript(value v, 0))
           fun gethdlr x = bind(x, PT.strongSubscript(exnptr, 0))
           fun getvar x = bind(x, PT.strongSubscript(varptr, 0))

           (* CPS Setter Primitives *)
           fun supdate(a,x) = PT.strongUpdate(value a, 0, makeTop(value x))
           fun wupdate(a,x) = PT.weakUpdate(value a, makeTop(value x))

           fun arrayupdate(a,i,x) = PT.weakUpdate(arrayptr a,value x) 

           fun assign(a,x) = supdate(a,x) 
           fun unboxedassign(a,x) = supdate(a,x) 
           fun update(a,i,x) = arrayupdate(a,i,x) 
           fun boxedupdate(a,i,x) = arrayupdate(a,i,x) 
           fun unboxedupdate(a,i,x) = arrayupdate(a,i,x) 
           fun numupdate(a,i,x) = arrayupdate(a,i,x) 
           fun numupdateF64(a,i,x) = arrayupdate(a,i,x) 
           fun sethdlr x = PT.strongUpdate(exnptr, 0, value x)
           fun setvar  x = PT.strongUpdate(varptr, 0, value x)

           fun infer(C.RECORD(rk,vs,x,k),hp) = 
                 (mkRecord(rk,x,vs,hp); infer(k,allocRecord(rk,vs,hp)))
             | infer(C.SELECT(i,v,x,cty,k),hp) = (select(i,v,x); infer(k,hp))
             | infer(C.OFFSET(i,v,x,k),hp) = (offset(i,v,x); infer(k,hp))
             | infer(C.APP(f,vs),hp) = apply(f,vs)
             | infer(C.FIX _,hp) = error "infer: FIX"
             | infer(C.SWITCH(v,x,ks),hp) = infers(ks,hp)
             | infer(C.BRANCH(p,_,x,k1,k2),hp) = (infer(k1,hp); infer(k2,hp))

               (* 
                * These things are misnamed! There is nothing pure about them! 
                *)
             | infer(C.PURE(P.objlength, [v], x, _, k), hp) = 
                 (objlength(x, v); infer(k, hp)) 
             | infer(C.PURE(P.length, [v], x, _, k), hp) = 
                 (length(x, v); infer(k, hp)) 
             | infer(C.PURE(P.subscriptv,[a,i],x,_,k),hp) = 
                 (subscriptv(x, a, i); infer(k, hp))
             | infer(C.PURE(P.pure_numsubscript{kind=P.INT 8},[a,i],x,_,k),hp) =
                 (pure_numsubscript(x, a, i); infer(k, hp))
             | infer(C.PURE(P.gettag, [v], x, _, k), hp) =
                 (gettag(x, v); infer(k, hp))
             | infer(C.PURE(P.mkspecial,[i,v],x,cty,k),hp) = 
                 (mkspecial(x,v,hp); infer(k,hp+8))
             | infer(C.PURE(P.makeref,[v],x,cty,k),hp) = 
                 (makeref(x,v,hp); infer(k,hp+8))
             | infer(C.PURE(P.fwrap,[v],x,cty,k),hp) = 
                 (fwrap(x,v,hp); infer(k,frecordSize(1,hp)))
             | infer(C.PURE(P.i32wrap,[v],x,cty,k),hp) = 
                 (i32wrap(x,v,hp); infer(k,hp+8))
             | infer(C.PURE(P.getcon,[v],x,_,k), hp) =
                 (getcon(x, v); infer(k, hp))
             | infer(C.PURE(P.getexn,[v],x,_,k), hp) =
                 (getexn(x, v); infer(k, hp))
             | infer(C.PURE(P.recsubscript,[a,i],x,_,k), hp) =
                 (recsubscript(x,a,i); infer(k, hp))
             | infer(C.PURE(P.raw64subscript,[a,i],x,_,k), hp) =
                 (raw64subscript(x,a,i); infer(k, hp))
             | infer(C.PURE(P.newarray0,_,x,cty,k),hp) = 
                 (newarray0(x,hp); infer(k,hp+array0Size))
             | infer(C.PURE(p,vs,x,cty,k),hp) = infer(k,hp)

             | infer(C.ARITH(a,vs,x,cty,k),hp) = infer(k,hp)

               (* Lookers *)
             | infer(C.LOOKER(P.!,[v],x,_,k),hp) = (deref(x,v); infer(k,hp))
             | infer(C.LOOKER(P.gethdlr,[],x,_,k),hp) = (gethdlr x; infer(k,hp))
             | infer(C.LOOKER(P.subscript,[a,i],x,_,k),hp) = 
                 (subscript(x,a,i); infer(k,hp))
             | infer(C.LOOKER(P.numsubscript{kind=P.INT 8},[a,i],x,_,k),hp) = 
                 (numsubscript8(x,a,i); infer(k,hp))
             | infer(C.LOOKER(P.numsubscript{kind=P.FLOAT 64},[a,i],x,_,k),hp) =
                 (numsubscriptf64(x,a,i); infer(k,hp))

             | infer(C.LOOKER(P.getvar,[],x,_,k),hp) = (getvar x; infer(k,hp))

             | infer(C.LOOKER(P.deflvar,[],x,cty,k),hp) = infer(k,hp) (* nop! *)
 
               (* Setters *)
             | infer(C.SETTER(P.assign, [a,v], k),hp) = 
                 (assign(a,v); infer(k,hp+storeListSize))
             | infer(C.SETTER(P.unboxedassign, [a,v], k),hp) = 
                 (unboxedassign(a,v); infer(k,hp))
             | infer(C.SETTER(P.update, [a,i,v], k),hp) = 
                 (update(a,i,v); infer(k,hp+storeListSize))
             | infer(C.SETTER(P.boxedupdate, [a,i,v], k), hp) = 
                 (boxedupdate(a,i,v); infer(k,hp+storeListSize))
             | infer(C.SETTER(P.unboxedupdate, [a,i,v], k), hp) =
                 (unboxedupdate(a,i,v); infer(k,hp))
             | infer(C.SETTER(P.numupdate{kind=P.INT _}, [a,i,v], k),hp) =
                 (numupdate(a,i,v); infer(k,hp))
             | infer(C.SETTER(P.numupdate{kind=P.FLOAT 64}, [a,i,v], k),hp) =
                 (numupdateF64(a,i,v); infer(k,hp))

             | infer(C.SETTER(P.sethdlr, [x], k), hp) = (sethdlr x; infer(k,hp))
             | infer(C.SETTER(P.setvar, [x], k), hp) = (setvar x; infer(k,hp))

                (* Apparently these are nops (see MLRiscGen.sml) *)
             | infer(C.SETTER(P.uselvar, [x], k), hp) = infer(k, hp)
             | infer(C.SETTER(P.acclink, _, k), hp) = infer(k, hp)
             | infer(C.SETTER(P.setmark, _, k), hp) = infer(k, hp)
             | infer(C.SETTER(P.free, [x], k), hp) = infer(k, hp)

             | infer(C.SETTER(P.setpseudo, _, k), hp) = 
                 (print "setpseudo not implemented\n"; infer(k, hp))
             | infer(e, hp) =  
                 (PPCps.prcps e; print "\n"; error "infer")
 
           and infers([],hp) = ()
             | infers(k::ks,hp) = (infer(k,hp); infers(ks,hp))
       in infer(cexp, 0)
       end

   in  if !Control.CG.memDisambiguate then
       (CPSRegions.reset();
        app defineFunction cpsFunctions;
        app process cpsFunctions;
        fn r => look r handle _ => top
       ) 
       else
       (fn _ => top)
   end  
end

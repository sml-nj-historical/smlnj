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
   val analyze : CPS.function list -> (CPS.lvar -> PointsTo.loc)
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
   fun record(n,hp)  =  n * 4 + 4 + hp
   fun frecord(n,hp) = 
       let val hp = if Word.andb(Word.fromInt hp,0w4) <> 0w0 then hp+8 else hp+4
       in  8*n + hp end
   fun vector(n,hp) = n * 4 + 16 + hp 

   fun allocRecord(C.RK_FBLOCK,vs,hp) = frecord(length vs,hp)
     | allocRecord(C.RK_FCONT,vs,hp) = frecord(length vs,hp)
     | allocRecord(C.RK_VECTOR,vs,hp) = vector(length vs,hp)
     | allocRecord(_,vs,hp) = record(length vs,hp)

   val storeListSize = 8
   val array0Size    = 20

   exception NotFound

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
         | sizeOf(C.SETTER(P.update,vs,k),hp) = sizeOf(k,hp+storeListSize)
         | sizeOf(C.SETTER(P.boxedupdate,vs,k),hp) = sizeOf(k,hp+storeListSize)
         | sizeOf(C.SETTER(_,vs,k),hp) = sizeOf(k,hp)
         | sizeOf(C.PURE(P.fwrap,vs,x,cty,k),hp) = sizeOf(k,frecord(1,hp))
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

       fun newRef _ = ref(PT.REF(newMem(),ref []))

       fun lookup x =
           look x handle _ =>
           let val l = newRef() in bind(x,l); l end

       fun process(fk, f, args, _, cexp) = 
       let (* process function f *)
           val argLocs = map (fn x => let val l = newRef() in bind(x,l); l end)
                           args
           val _ = PT.app(lookup f,argLocs)

           (* create a table of allocation offset locations *)
           val table = Array.tabulate(sizeOf(cexp, 0) div 4, newRef)

           fun select(i,C.VAR v,x) = bind(x,PT.pi(lookup v,i))
             | select(i,_,x)       = ()

           fun offset(i,C.VAR v,x) = bind(x,PT.offset(lookup v,i))
             | offset(i,_,x)       = ()

           fun value (C.VAR v) = lookup v
             | value _         = newRef()

           fun apply(C.VAR f,args) = PT.app(lookup f,map value args)
             | apply _             = ()

           fun mkrecord(x,vs,hp) =
           let fun g([],i)           = []
                 | g((v,path)::vs,i) = 
                   let val l  = Array.sub(table,i)
                       val l' = get(v,path)
                   in  PT.unify(l,l'); l::g(vs,i+1) end

               and get(v,C.OFFp 0) = value v
                 | get(v,C.OFFp n) = PT.offset(value v,n)
                 | get(v,C.SELp(n,path)) = PT.pi(get(v,path),n)

               val i = Word.toInt(Word.>>(Word.fromInt hp,0w2))
               val r = PT.record(Array.sub(table,i)::g(vs,i+1))
           in  bind(x,r)
           end

           fun infer(C.RECORD(rk,vs,x,k),hp) = 
                 (mkrecord(x,vs,hp); infer(k,allocRecord(rk,vs,hp)))
             | infer(C.SELECT(i,v,x,cty,k),hp) = (select(i,v,x); infer(k,hp))
             | infer(C.OFFSET(i,v,x,k),hp) = (offset(i,v,x); infer(k,hp))
             | infer(C.APP(f,vs),hp) = apply(f,vs)
             | infer(C.FIX _,hp) = error "infer: FIX"
             | infer(C.SWITCH(v,x,ks),hp) = infers(ks,hp)
             | infer(C.BRANCH(p,_,x,k1,k2),hp) = (infer(k1,hp); infer(k2,hp))
             | infer(C.SETTER(P.update,vs,k),hp) = infer(k,hp+storeListSize)
             | infer(C.SETTER(P.boxedupdate,vs,k),hp) = 
                  infer(k,hp+storeListSize)
             | infer(C.SETTER(P.numupdate{kind=P.FLOAT 64}, [a,i,v], k),hp) =
                  infer(k,hp)
             | infer(C.SETTER(_,vs,k),hp) = infer(k,hp)
               (* 
                * These things are misnamed! There is nothing pure about them! 
                *)
             | infer(C.PURE(P.mkspecial,[i,v],x,cty,k),hp) = 
                 (bind(x,value v); infer(k,hp+8))
             | infer(C.PURE(P.fwrap,[u],x,cty,k),hp) = 
                 (bind(x,value u); infer(k,frecord(1,hp)))
             | infer(C.PURE(P.i32wrap,[u],x,cty,k),hp) = 
                 (bind(x,value u); infer(k,hp+8))
             | infer(C.PURE(P.makeref,[v],x,cty,k),hp) = 
                 (bind(x,value v); infer(k,hp+8))
             | infer(C.PURE(P.newarray0,_,x,cty,k),hp) = 
                 (bind(x,newRef()); infer(k,hp+array0Size))
             | infer(C.PURE(p,vs,x,cty,k),hp) = infer(k,hp)
             | infer(C.ARITH(a,vs,x,cty,k),hp) = infer(k,hp)
             | infer(C.LOOKER(lk,vs,x,cty,k),hp) = infer(k,hp)

           and infers([],hp) = ()
             | infers(k::ks,hp) = (infer(k,hp); infers(ks,hp))
       in infer(cexp, 0)
       end

       val _   = PT.reset(Cells.newCell Cells.MEM)
       val top = ref(PT.NAMED("mem",PT.newTop()))
   in  if !Control.CG.memDisambiguate then
       (app process cpsFunctions;
        fn r => look r handle _ => top
       ) 
       else
       (fn _ => top)
   end 
end

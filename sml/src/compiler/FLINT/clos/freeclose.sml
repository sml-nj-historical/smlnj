(* Copyright 1996 by Bell Laboratories *)
(* freeclose.sml *)

(***************************************************************************
 *                                                                         *
 * freemapClose                                                            *
 *                                                                         *
 *    Produces a free variable mapping at each function binding.           *
 *    The mapping includes the functions bound at the FIX, but not the     *
 *    arguments of the function.                                           *
 *                                                                         *
 *    Side-effect: all fundefs that are never referenced are removed       *
 *                                                                         *
 ***************************************************************************)
signature FREECLOSE =
  sig
    type snum
    type fvinfo
    val freemapClose : CPS.function -> (CPS.function * (CPS.lvar -> snum)
                                        * (CPS.lvar -> fvinfo)
                                        * (CPS.lvar -> bool))
  end

structure FreeClose : FREECLOSE = struct

local
  open Access CPS SortedList
  structure LV = LambdaVar
in

(***************************************************************************
 *  Misc and utility functions                                             *
 ***************************************************************************)
val say = Control.Print.say
fun vp v = say(LV.lvarName(v))

fun addvL(v,NONE) = NONE
  | addvL(v,SOME l) = SOME(enter(v,l))

val enter = fn (VAR x,y) => enter(x,y) | (_,y) => y
val error = ErrorMsg.impossible
fun warn s = () (* app say ["WARNING: ",s,"\n"] *)

fun addL(v,NONE) = NONE
  | addL(v,SOME l) = SOME(enter(v,l))

fun overL(r,NONE) = NONE
  | overL(r,SOME l) = SOME (merge(r,l))

fun mergeL(NONE,r) = r
  | mergeL(l,NONE) = l
  | mergeL(SOME l,SOME r) = SOME (merge(l,r))

fun removeL(vl,NONE) = NONE
  | removeL(vl,SOME r) = SOME (remove(vl,r))

fun rmvL(v,NONE) = NONE 
  | rmvL(v,SOME r) = SOME (rmv(v,r))

fun clean l = 
  let fun vars(l, (VAR x) :: rest) = vars(x::l, rest)
	| vars(l, _::rest) = vars(l,rest)
	| vars(l, nil) = uniq l
   in vars(nil, l)
  end

fun filter p vl = 
  let fun f(x::r,l) = if p x then f(r,x::l) else f(r,l)
        | f([],l) = rev l
   in f(vl,[])
  end

fun exists pred l = 
  let fun f(a::r) = if pred a then true else f r
        | f [] = false
   in f l
  end

fun partition f l = 
  foldr (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b)) ([], []) l

val infinity = 1000000000
fun minl l = 
  let fun f(i,nil) = i 
        | f(i,j::r) = if i < j then f(i,r) else f(j,r)
   in f(infinity,l)
  end

fun bfirst(P.boxed | P.pneq | P.strneq | P.cmp{oper=P.neq,...}) = true
  | bfirst _ = false

fun bsecond(P.unboxed | P.peql | P.streq | P.cmp{oper=P.eql,...}) = true
  | bsecond _ = false

(** datatype used to represent the free variable information **)
type vnum = lvar * int * int  (* lvar and first-use-sn and last-use-sn *)
type snum = int       (* stage number *)
type loopv = lvar list option
type fvinfo = {fv : vnum list, (* list of sorted free variables *)
               lv : loopv,     (* list of free variables on the loop path *)
               sz : int * int} (* estimated frame-size of the current fun *) 

fun freemapClose fe = let

(***************************************************************************
 * Modify the fun_kind information for each fundef, new kind includes,     *
 *                                                                         *
 *      (1) KNOWN_CONT       known continuation function                   *
 *      (2) KNOWN_TAIL       known tail-recursive function                 *
 *      (3) KNOWN            general known function                        *
 *      (4) CONT             general continuation function                 *
 *      (5) ESCAPE           general escaping user function                *
 *      (6) KNOWN_REC        mutually recursive known function             *
 *                                                                         *
 ***************************************************************************)
val escapes = Intset.new()
val escapesP = Intset.mem escapes
fun escapesM(VAR v) = Intset.add escapes v
  | escapesM _ = ()

val users = Intset.new()
val usersP = Intset.mem users
val usersM = Intset.add users

val known = Intset.new()
val knownP = Intset.mem known
val knownM = Intset.add known
fun knownK k = (k <> CONT) andalso (k <> ESCAPE)

val contset = Intset.new()
val contP = Intset.mem contset
val contM = Intset.add contset
fun contK k = (k = CONT) orelse (k = KNOWN_CONT) (* continuation funs ? *)
fun econtK k = (k = CONT)                (* escaping continuation funs ? *)

fun fixkind(fe as (CONT,f,vl,cl,ce)) = 
       if escapesP f then (contM f; fe)
       else (knownM f; contM f; (KNOWN_CONT,f,vl,cl,ce))
  | fixkind(fe as (fk,f,vl,cl as (CNTt::_),ce)) = 
       if escapesP f then (usersM f; (ESCAPE,f,vl,cl,ce))
       else (knownM f; (KNOWN_REC,f,vl,cl,ce))
  | fixkind(fe as (fk,f,vl,cl,ce)) = 
       if escapesP f then (vp f; say " ***** \n";
             error "escaping-fun has zero cont, freeclose.sml")
       else (knownM f; (KNOWN_TAIL,f,vl,cl,ce))

fun procfix(fk,f,vl,cl,ce) = (fk,f,vl,cl,proc ce)
and proc(ce) =
      case ce 
       of FIX(fl,body) =>
	   let val body' = proc body
               val nfl = map fixkind (map procfix fl)
               
               (* Due to possible eta-splits of continuation functions, 
                * since it's always that CONT funs calls KNOWN_CONT funs, 
                * we split them into two FIXes, so that each FIX only 
                * contains at most one continuation definitions.
                *)
               val (fl1,fl2) = partition (econtK o #1) nfl 
            in case (fl1,fl2) 
                of ([],_) => FIX(fl2,body')
                 | (_,[]) => FIX(fl1,body')
                 | _ => FIX(fl2,FIX(fl1,body'))
	   end
        | APP(v,args) => (app escapesM args; ce)
	| SWITCH(v,c,l) => SWITCH(v,c,map proc l)
	| RECORD(rk,l,w,ce) => 
           (app (escapesM o #1) l; RECORD(rk,l,w,proc ce))
	| SELECT(i,v,w,t,ce) => SELECT(i,v,w,t,proc ce)
	| OFFSET(i,v,w,ce) => OFFSET(i,v,w,proc ce)
	| LOOKER(p,vl,w,t,ce) => 
           (app escapesM vl; LOOKER(p,vl,w,t,proc ce))
	| ARITH(p,vl,w,t,ce) => 
           (app escapesM vl; ARITH(p,vl,w,t,proc ce))
	| PURE(p,vl,w,t,ce) => 
           (app escapesM vl; PURE(p,vl,w,t,proc ce))
	| SETTER(p,vl,ce) => 
           (app escapesM vl; SETTER(p,vl,proc ce))
	| BRANCH(p,vl,c,e1,e2) =>
           (app escapesM vl; BRANCH(p,vl,c,proc e1,proc e2))

val fe' = procfix fe


(***************************************************************************
 * Build the call graph and compute the scc number                         *
 ***************************************************************************)
exception Unseen
type info = {dfsnum : int ref, sccnum : int ref, edges : lvar list}
val m : info Intmap.intmap = Intmap.new(32,Unseen)
val lookup = Intmap.map m
val total : lvar list ref = ref nil

fun addinfo(f,vl) = (total := (f :: (!total));
                     Intmap.add m (f,{dfsnum=ref ~1,sccnum=ref ~1,edges=vl}))
fun KUC x = (contP x) orelse (knownP x) orelse (usersP x)
fun EC x = (contP x) orelse (escapesP x)

fun makenode (_,f,_,_,body) =
  let fun edges (RECORD(_,_,_,e)) = edges e
	| edges (SELECT(_,_,_,_,e)) = edges e
	| edges (OFFSET(_,_,_,e)) = edges e
	| edges (SWITCH(_,_,el)) = foldmerge (map edges el) 
        | edges (SETTER(P.sethdlr,vl,e)) = 
            merge(filter KUC (clean vl),edges e)
	| edges (SETTER(_,_,e)) = edges e
	| edges (LOOKER(_,_,_,_,e)) = edges e
	| edges (ARITH(_,_,_,_,e)) = edges e
        | edges (PURE(_,_,_,_,e)) = edges e
	| edges (BRANCH(_,_,_,a,b)) = merge(edges a,edges b)
        | edges (APP(u, ul)) = filter KUC (clean (u::ul))
	| edges (FIX(fl,b)) = (app makenode fl; edges b)
   in addinfo(f,edges body)
  end 

val compnums = ref 0 and id = ref 0
val stack : (int * int ref) list ref = ref nil
fun scc nodenum =
  let fun newcomp(c,(n,sccnum)::rest) = 
	    (sccnum := c; 
             if n=nodenum then rest else newcomp(c,rest))
        | newcomp _ = error "newcomp in freeclose in the closure phase"

      val info as {dfsnum as ref d, sccnum, edges} = lookup nodenum

   in if d >= 0 then if (!sccnum >= 0) then infinity else d 
      else (let val v = !id before (id := !id+1)
                val _ = (stack := (nodenum, sccnum) :: !stack;
                         dfsnum := v)
                val b = minl (map scc edges)
             in if v <= b 
                then let val c = !compnums before (compnums := !compnums+1)
                         val _ = (stack := newcomp(c,!stack))
                      in infinity (* v *)
                     end
                else b
            end)
  end

val _ = makenode(fe')               (* Build the call graph *)
val _ = app (fn x => (scc x; ())) (!total)   (* Compute the scc number *)
val sccnum = ! o #sccnum o lookup
fun samescc(x,n) = if n < 0 then false else ((sccnum x) = n)

(***>>
   fun plist p l = (app (fn v => (say " "; p v)) l; say "\n")
   val ilist = plist vp
   val _ = app (fn v => (vp v; say " edges : " ;
                         ilist(#edges(lookup v));
                         say "****   sccnum is   "; 
                         say (Int.toString(sccnum v)); say "\n")) (!total)
<<***)



(***************************************************************************
 * Utility functions for lists of free variable unit, each unit "vnum"     *
 * contains three parts, the lvar, the first-use-sn and the last-use-sn    *
 ***************************************************************************)
val V2L = let fun h (s:vnum) = #1 s 
           in map h           (* given a vnum list, return an lvar list *)
          end

(* add a single lvar used at stage n *)
fun addsV(VAR v,n,l) = 
     let fun h(v,[]) = [(v,n,n)]
           | h(v,l as ((u as (x,a,b))::r)) = 
              if x < v then u::(h(v,r))
              else if x = v then ((x,Int.min(a,n),Int.max(a,n))::r)
                   else ((v,n,n)::l)
      in h(v,l)
     end
  | addsV(_,_,l) = l


(* remove a single lvar *)
fun rmvsV(v,[]) = []
  | rmvsV(v,l as ((u as (x,_,_))::r)) = 
     if x < v then u::(rmvsV(v,r))
     else if x = v then r
          else l

(* remove a list of lvars *)
fun removeV(vl,l) = 
  let fun h(l1 as (x1::r1),l2 as ((u2 as (x2,_,_))::r2)) = 
            if x2 < x1 then u2::(h(l1,r2))
            else if x2 > x1 then h(r1,l2)
                 else h(r1,r2)
        | h([],l2) = l2
        | h(l1,[]) = []
   in h(vl,l)
  end

(* add a list of lvars used at stage n *)
fun addV(vl,n,l) = 
  let fun h(l1 as (x1::r1), l2 as ((u2 as (x2,a2,b2))::r2)) =
            if (x1 < x2) then (x1,n,n)::(h(r1,l2))
            else if (x1 > x2) then u2::(h(l1,r2))
                 else (x1,Int.min(n,a2),Int.max(n,b2))::(h(r1,r2))
        | h(l1,[]) = map (fn x => (x,n,n)) l1
        | h([],l2) = l2
   in h(vl,l)
  end

(* merge two lists of free var unit (exclusively) *)
fun mergePV(n,l1,l2) = 
  let fun h(l1 as ((x1,a1,b1)::r1),  l2 as ((x2,a2,b2)::r2)) =
            if (x1 < x2) then (x1,n,n)::(h(r1,l2))
            else if (x1 > x2) then (x2,n,n)::(h(l1,r2))
                 else if (b1 = b2) then 
                          (x1,Int.min(a1,a2),b1)::(h(r1,r2))
                      else (x1,n,n)::(h(r1,r2))
        | h(l1,[]) = map (fn (x,_,_) => (x,n,n)) l1
        | h([],l2) = map (fn (x,_,_) => (x,n,n)) l2
   in h(l1,l2)
  end

(* merge two lists of free var unit (with union) *)
fun mergeUV(l1 : (lvar*int*int) list,l2) =
  let fun h(l1 as ((u1 as (x1,a1,b1))::r1), l2 as ((u2 as (x2,a2,b2))::r2)) =
            if (x1 < x2) then u1::(h(r1,l2))
            else if (x1 > x2) then u2::(h(l1,r2))
                 else (x1,Int.min(a1,a2),Int.max(b1,b2))::(h(r1,r2))
        | h(l1,[]) = l1
        | h([],l2) = l2
   in h(l1,l2)
  end

(* fold merge lists of free vars (exclusively) *)
fun foldUV(l,b) = foldr mergeUV b l

(* lay a list of free var unit over another list of free var unit *)
fun overV(n,l1,l2) = 
  let fun h(l1 as ((u1 as (x1,_,_))::r1),  l2 as ((x2,_,_)::r2)) =
            if (x1 < x2) then u1::(h(r1,l2))
            else if (x1 > x2) then (x2,n,n)::(h(l1,r2))
                 else u1::(h(r1,r2))
        | h(l1,[]) = l1
        | h([],l2) = map (fn (x,_,_) => (x,n,n)) l2
   in h(l1,l2)
  end
  


(***************************************************************************
 * Two hash tables (1) lvar to stage number                                *
 *                 (2) lvar to freevar information                         *
 ***************************************************************************)
exception STAGENUM
val snum : snum Intmap.intmap = Intmap.new(32,STAGENUM)
val addsn = Intmap.add snum   (* add the stage number for a fundef *)    
val getsn = Intmap.map snum   (* get the stage number of a fundef *)

fun findsn(v,d,[]) = (warn ("Fundef " ^ (LV.lvarName v)
                            ^ " unused in freeClose"); d)
  | findsn(v,d,(x,_,m)::r) = 
      if v > x then findsn(v,d,r) 
      else if v = x then m 
           else (warn ("Fundef " ^ (LV.lvarName v) ^ 
                       " unused in freeClose"); d)

fun findsn2(v,d,[]) = d
  | findsn2(v,d,(x,_,m)::r) = 
      if v > x then findsn2(v,d,r)
      else if v = x then m else d
             

exception FREEVMAP
val vars : fvinfo Intmap.intmap = Intmap.new(32,FREEVMAP)

fun addEntry(v,l,x,s) = Intmap.add vars (v,{fv=l,lv=x,sz=s})
val freeV = Intmap.map vars    (* get the freevar info *)
val loopV = #lv o freeV        (* the free variables on the loop path *)

(***>>
  val vars : (lvar list * (lvar list option)) Intmap.intmap 
                                               = Intmap.new(32, FREEVMAP)
  val freeV = Intmap.map vars 
  fun loopV v = (#2 (freeV v)) handle FREEVMAP => error "loopV in closure"
<<***)

(***************************************************************************
 * Split the pseudo-mutually-recursive bindings, a temporary hack.         *
 *                                                                         *
 * TODO: need to add code on identify those KNOWN_REC kind functions       *
 *       check the older version of this file for details                  *
 ***************************************************************************)
fun knownOpt ([],_,_,_,_) = error "knownOpt in closure 4354"
  | knownOpt (flinfo,died,freeb,gszb,fszb) = 
      let val newflinfo = 
            let val roots = filter (member died) (V2L freeb)
                val graph = map (fn ((_,f,_,_,_),free,_,_) => 
                                   (f,filter (member died) (V2L free))) flinfo
                fun loop(old) = 
                  let val new = 
                        foldr (fn ((f,free),total) => 
                           if member old f then merge(free,total) else total)
                        old graph
                   in if length(new) = length(old) then new else loop(new)
                  end

                val nroots = loop(roots)
             in filter (fn ((_,f,_,_,_),_,_,_) => member nroots f) flinfo
            end

          val (nfl,freel,gsz,fsz) =
            let val (known,other) = 
                       partition (fn ((KNOWN_REC,_,_,_,_),_,_,_) => true
                                   | _ => false) newflinfo

                val known' = 
                  case known 
                   of u as [((_,v,args,cl,body),free,gsz,fsz)] => 
                         (if member (V2L free) v then u
                          else [((KNOWN,v,args,cl,body),free,gsz,fsz)])
                    | z => z

                fun g((fe,vn,gsz',fsz'),(fl,vl,gsz,fsz)) =
                      (fe::fl,vn::vl,Int.max(gsz',gsz),Int.max(fsz',fsz))
             in foldr g ([],[],gszb,fszb) (known'@other)
            end

          val header = case nfl of [] => (fn ce => ce)
                                 | _ => (fn ce => FIX(nfl,ce))

       in (header, freel, gsz, fsz)
      end

(***************************************************************************
 * The following procedure does five things:                               *
 *                                                                         *
 *  (1) Install a stage number for each function definition                *
 *  (2) Collecting the free variable information for each fundef           *
 *  (3) Infer the live range of each free variable at each fundef          *
 *  (4) Infer the set of free variables on the looping path                *
 *  (5) Do the simple branch-prediction transformation                     *
 *                                                                         *
 * TODO: better branch-prediction heauristics will help the merge done     *
 *       at each SWITCH and BRANCH                                         *
 ***************************************************************************)

(*** major gross hack here ***)
val ekfuns = Intset.new()
val ekfunsP = Intset.mem ekfuns
val ekfunsM = Intset.add ekfuns

fun freefix (sn,freeb) (fk,f,vl,cl,ce) =
     let val (ce',ul,wl,gsz,fsz) = 
           if contK fk then 
             (let val n = findsn(f,sn,freeb)
                  val nn = if econtK fk then n+1 else n
               in addsn(f,nn); freevars(sccnum f,nn,ce) 
              end)
           else if knownK fk then (addsn(f,sn); freevars(sccnum f,sn,ce))
                else (addsn(f,sn+1); freevars(~1,sn+1,ce))
         val args = uniq vl
         val l = removeV(args,ul)
         val z = removeL(args,wl)

         (*** the following is a gross hack, needs more work ***)
         val nl = 
           if ((findsn2(f,sn,l)) <= sn) then l
           else (foldr (fn ((x,i,j),z) => 
                              (if knownP x then ekfunsM x else (); 
                               (x,i+1,j+1)::z)) [] l)

         val _ = addEntry(f,l,z,(gsz,fsz))
         val (gsz',fsz') = 
           if econtK fk then   (* only count escaping functions *)
             (let val gn = length l (**** NEED MORE WORK HERE ****)
               in (Int.max(gn,gsz),fsz)
              end) 
           else (0,0)

      in ((fk,f,vl,cl,ce'),nl,gsz',fsz')
     end

and freevars(n,sn,ce) =
  case ce 
   of FIX(fl,body) =>
       let val died = uniq(map #2 fl) 
           val (body',freeb,wl,gszb,fszb) = freevars(n,sn,body)
           val flinfo = map (freefix (sn,freeb)) fl
           val (header,freel,gsz,fsz) = knownOpt(flinfo,died,freeb,gszb,fszb)
           val free = removeV(died,foldUV(freel,freeb))
           val nwl = case wl 
             of NONE => NONE
              | SOME l => 
                  (let fun h(x,l) = if member died x then mergeL(loopV x,l) 
                                    else addvL(x,l) 
                    in removeL(died,foldr h (SOME []) l)
                   end)
        in (header(body'),free,nwl,gsz,fsz)
       end
    | APP(v,args) => 
       let val free = clean(v::args)
           val fns = filter KUC free
           val wl = if (exists (fn x => samescc(x,n)) fns) then SOME free
                    else NONE
           val freeb = addV(free,sn,[])
        in (ce,freeb,wl,0,0)
       end
    | SWITCH(v,c,l) =>  (* add branch prediction heauristics in the future *)
       let fun freelist(ce,(el,free1,free2,wl,gsz1,fsz1,gsz2,fsz2)) =
             let val (ce',free',wl',gsz',fsz') = freevars(n,sn,ce)
              in case wl' 
                  of NONE => 
                      (ce'::el,free1,mergePV(sn,free',free2),wl,
                       gsz1,fsz1,Int.max(gsz2,gsz'),Int.max(fsz2,fsz'))
                   | SOME _ => 
                      (ce'::el,mergeUV(free',free1),free2,mergeL(wl',wl),
                       Int.max(gsz1,gsz'),Int.max(fsz1,fsz'),gsz2,fsz2)
             end
           val (l',free1,free2,wl,gsz1,fsz1,gsz2,fsz2) = 
                          foldr freelist ([],[],[],NONE,0,0,0,0) l
           val (free,gsz,fsz) = case wl
             of NONE => (free2,gsz2,fsz2)
              | SOME _ => (overV(sn,free1,free2),gsz1,fsz1)

        in (SWITCH(v,c,l'),addsV(v,sn,free),addL(v,wl),gsz,fsz)
       end
(*    | SWITCH(v,c,l) =>  (* add branch prediction heauristics in the future *)
       let fun freelist(ce,(el,free,wl,gsz,fsz)) =
             let val (ce',free',wl',gsz',fsz') = freevars(n,sn,ce)
                 val ngsz = Int.max(gsz,gsz')
                 val nfsz = Int.max(fsz,fsz')
              in (ce'::el,mergePV(sn,free',free),mergeL(wl',wl),ngsz,nfsz)
             end
           val (l',freel,wl,gsz,fsz) = foldr freelist ([],[],NONE,0,0) l
        in (SWITCH(v,c,l'),addsV(v,sn,freel),addL(v,wl),gsz,fsz)
       end
*)
    | RECORD(rk,l,w,ce) => 
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean (map #1 l)   
           val free' = addV(new,sn,rmvsV(w,free))
           val wl' = overL(new, rmvL(w,wl))
        in (RECORD(rk,l,w,ce'),free',wl',gsz,fsz)
       end
    | SELECT(i,v,w,t,ce) =>
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val free' = addsV(v,sn,rmvsV(w,free))
           val wl' = addL(v,rmvL(w,wl))
        in (SELECT(i,v,w,t,ce'),free',wl',gsz,fsz)
       end
    | OFFSET(i,v,w,ce) =>
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val free' = addsV(v,sn,rmvsV(w,free))
           val wl' = addL(v,rmvL(w,wl))
        in (OFFSET(i,v,w,ce'),free',wl',gsz,fsz)
       end
    | LOOKER(p,vl,w,t,ce) => 
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean vl
           val free' = addV(new,sn,rmvsV(w,free))
           val wl' = overL(new,rmvL(w,wl))
        in (LOOKER(p,vl,w,t,ce'),free',wl',gsz,fsz)
       end
    | ARITH(p,vl,w,t,ce) => 
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean vl
           val free' = addV(new,sn,rmvsV(w,free))
           val wl' = overL(new,rmvL(w,wl))
        in (ARITH(p,vl,w,t,ce'),free',wl',gsz,fsz)
       end
    | PURE(p,vl,w,t,ce) => 
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean vl
           val free' = addV(new,sn,rmvsV(w,free))
           val wl' = overL(new,rmvL(w,wl))
        in (PURE(p,vl,w,t,ce'),free',wl',gsz,fsz)
       end
    | SETTER(p as P.sethdlr,vl,ce) =>
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean vl
           val free' = addV(new,sn,free)
           val fns = filter KUC new
           val wl' = if (exists(fn x => samescc(x,n)) fns) 
                     then mergeL(SOME new,wl) else overL(new,wl)
        in (SETTER(p,vl,ce'),free',wl',gsz,fsz)
       end
    | SETTER(p,vl,ce) => 
       let val (ce',free,wl,gsz,fsz) = freevars(n,sn,ce)
           val new = clean vl
           val free' = addV(new,sn,free)
           val wl' = overL(new,wl)
        in (SETTER(p,vl,ce'),free',wl',gsz,fsz)
       end
    | BRANCH(p,vl,c,e1,e2) =>
       let val (e1',free1,wl1,gsz1,fsz1) = freevars(n,sn,e1)
           val (e2',free2,wl2,gsz2,fsz2) = freevars(n,sn,e2)
           val new = clean vl
           val wl = overL(new,mergeL(wl1,wl2))
        in case (wl1,wl2)
            of (NONE,SOME _) => 
                 (let val free = addV(new,sn,overV(sn,free2,free1))
                   in (BRANCH(P.opp p,vl,c,e2',e1'),free,wl,gsz2,fsz2)
                  end)
             | (SOME _,NONE) => 
                 (let val free = addV(new,sn,overV(sn,free1,free2))
                   in (BRANCH(p,vl,c,e1',e2'),free,wl,gsz1,fsz1)
                  end)
             | _ => 
                 (let val free = case wl1 
                        of (SOME _) => addV(new,sn,mergeUV(free1,free2))
                         | _ => 
                            (if bfirst(p) then 
                                addV(new,sn,overV(sn,free1,free2))
                             else if bsecond(p) then
                                     addV(new,sn,overV(sn,free2,free1))
                                  else addV(new,sn,mergePV(sn,free1,free2)))
                      val gsz = Int.max(gsz1,gsz2)
                      val fsz = Int.max(fsz1,fsz2)
                   in (BRANCH(p,vl,c,e1',e2'),free,wl,gsz,fsz)
                  end)
       end

 in (#1(freefix (0,[]) fe'), getsn, freeV, ekfunsP)
end (* function freemapClose *)

(*
val freemapClose = Stats.doPhase(Stats.makePhase "Compiler 079 freemapClose")
                     freemapClose
*)

end
end (* structure FreeClose *)


(*
 * $Log: freeclose.sml,v $
 * Revision 1.1  1998/05/20 18:36:49  george
 *   New code that avoids sharing a single closure among
 *   mututually recursive functions. This way, we'll no longer
 *   have pointers that point to the middle of a record.
 * 						-- zsh
 *
 *)

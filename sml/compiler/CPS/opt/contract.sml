(* contract.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(*
Transformations performed by the contracter:

TRANSFORMATION:                       Click:   Compiler.Control.CG flag:
------------------------------------------------------------------------
Inlining functions that are used once   e      betacontract
Cascaded inlining of functions          q
The IF-idiom                            E      ifidiom
Unify BRANCHs                           z      branchfold
Constant folding:
 SELECTs from known RECORDs             d
 Handler operations                    ijk     handlerfold
 SWITCH expressions                     h      switchopt
 ARITH expressions              FGHIJKLMNOPQX  arithopt
 PURE expressions          RSTUVWYZ0123456789  arithopt
 BRANCH expressions                   nopvw    comparefold

Dead variable elimination:         [down,up]       [down,up]
 RECORDs                              [b,B1,B2]    [deadvars,deadup]
 SELECTs                              [c,s]        [deadvars,deadup]
 Functions                            [g,f]
 LOOKERs                              [m,*]        [deadvars,deadup]
 PUREs                                [m,*]        [deadvars,deadup]
 Arguments                            [D, ]        [dropargs, ]

Conversion Primops:
 testu					U(n)
 test					T(n)
 copy					C(n)
 extend					X(n)
 trunc					R(n)
*)

signature CONTRACT = sig
  val contract : {function: CPS.function,
                  click: string -> unit,
                  last: bool,
                  size: int ref}
                  -> CPS.function
end (* signature CONTRACT *)

functor Contract(MachSpec : MACH_SPEC) : CONTRACT =
struct

open CPS
structure LT = LtyExtern
structure LV = LambdaVar
structure CA = ConstArith

structure CG = Control.CG

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Contract: " ^ s)

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

(* integer types/values *)
local
  val tt = {sz = Target.defaultIntSz, tag = true}
  fun bt sz = {sz = sz, tag = false}
in
val tagIntTy = NUMt tt
fun tagInt n = NUM{ival = n, ty = tt}
fun tagInt' n = tagInt(IntInf.fromInt n)
fun boxIntTy sz = NUMt(bt sz)
fun boxInt {ival, ty} = NUM{ival = ival, ty = bt ty}
end

(* get the size of an integer operation *)
fun sizeOfKind (P.INT sz) = sz
  | sizeOfKind (P.UINT sz) = sz
  | sizeOfKind (P.FLOAT _) = bug "sizeOfKind(FLOAT _)"

exception ConstFold

fun map1 f (a,b) = (f a, b)

fun sameName (x, VAR y) = LV.sameName(x,y)
  | sameName (x, LABEL y) = LV.sameName(x,y)
  | sameName _ = ()

fun complain(t1,t2,s) =
  (say (s^"  ____ Type conflicting while contractions =====> \n    ");
   say (LT.lt_print t1); say "\n and   \n    "; say (LT.lt_print t2);
   say "\n \n";
   say "_____________________________________________________ \n")

fun checklty s (t1,t2) =  ()
(*
  let fun g (LT.INT, LT.INT) = ()
        | g (LT.INT32, LT.INT32) = ()
        | g (LT.BOOL, LT.BOOL) = ()
        | g (LT.INT, LT.BOOL) = ()
        | g (LT.BOOL, LT.INT) = ()
        | g (LT.REAL, LT.REAL) = ()
        | g (LT.SRCONT, LT.SRCONT) = ()
        | g (LT.BOXED, LT.BOXED) = ()
        | g (LT.RBOXED, LT.RBOXED) = ()
        | g (LT.INT, LT.RECORD nil) = ()
        | g (LT.RECORD nil, LT.INT) = ()
        | g (LT.BOXED, LT.RBOXED) = ()         (* this is temporary *)
        | g (LT.RBOXED, LT.BOXED) = ()         (* this is temporary *)
        | g (LT.ARROW(t1,t2),LT.ARROW(t1',t2')) =
             (g(LT.out t1,LT.out t1'); g(LT.out t2, LT.out t2'))
        | g (LT.RECORD l1,LT.RECORD l2) =
             ListPair.appEq g (map LT.out l1, map LT.out l2)
        | g (LT.CONT t1,LT.CONT t2) = g(LT.out t1,LT.out t2)
        | g (t1,t2) = complain(LT.inj t1, LT.inj t2,"CTR *** "^s)
  in  g(LT.out t1, LT.out t2)
  end
*)

fun equalUptoAlpha(ce1,ce2) =
  let fun equ pairs =
        let fun same(VAR a, VAR b) =
	          let fun look((x,y)::rest) = a=x andalso b=y orelse look rest
		        | look nil = false
		  in  a=b orelse look pairs
		  end
              | same(LABEL a, LABEL b) = same(VAR a, VAR b)
              | same(NUM i, NUM j) = (#ty i = #ty j) andalso (#ival i = #ival j)
              | same(REAL a, REAL b) = (#ty a = #ty b) andalso RealLit.same(#rval a, #rval b)
              | same(STRING a, STRING b) = a=b
	      | same(a,b) = false
	    fun sameField ((a, ap : accesspath), (b, bp)) = (ap = bp) andalso same(a, b)
	    fun samewith p = equ (p::pairs)
	    fun samewith' args =
		equ (ListPair.foldr (fn ((w, _), (w', _), l) => (w,w')::l)
				    pairs args)
            fun all2 f (e::r,e'::r') = f(e,e') andalso all2 f (r,r')
              | all2 f (nil,nil) = true
              | all2 f _ = false
            val rec sameexp =
	     fn (SELECT(i,v,w,_,e),SELECT(i',v',w',_,e')) =>
		   i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (RECORD(k,vl,w,e),RECORD(k',vl',w',e')) =>
		   (k = k')
		   andalso ListPair.allEq sameField (vl, vl')
                   andalso samewith (w,w') (e,e')
              | (OFFSET(i,v,w,e),OFFSET(i',v',w',e')) =>
		   i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (SWITCH(v,c,el),SWITCH(v',c',el')) =>
		   same(v,v') andalso all2 (samewith(c,c')) (el,el')
	      | (APP(f,vl),APP(f',vl')) =>
                   same(f,f') andalso all2 same (vl,vl')
              | (FIX(l,e),FIX(l',e')) => (* punt! *) false
	      | (BRANCH(i,vl,c,e1,e2),BRANCH(i',vl',c',e1',e2')) =>
		   i=i' andalso all2 same (vl,vl')
		   andalso samewith(c,c') (e1,e1')
		   andalso samewith(c,c') (e2,e2')
	      | (LOOKER(i,vl,w,_,e),LOOKER(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (SETTER(i,vl,e),SETTER(i',vl',e')) =>
		   i=i' andalso all2 same (vl,vl') andalso sameexp(e,e')
	      | (ARITH(i,vl,w,_,e),ARITH(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (PURE(i,vl,w,_,e),PURE(i',vl',w',_,e')) =>
		   i=i' andalso all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | (RCC(k,l,p,vl,wtl,e),RCC(k',l',p',vl',wtl',e')) =>
		(* We don't need to compare protocol info:  The protocols are
		 * the same iff the functions and arguments are the same. *)
		k = k' andalso l = l' andalso
                all2 same (vl,vl') andalso samewith'(wtl,wtl')(e,e')
	      | _ => false
        in  sameexp
        end
  in  equ nil (ce1,ce2)
  end

datatype info
  = FNinfo of {
	args: lvar list,
	body : cexp option ref,
	specialuse: int ref option ref,
	liveargs : bool list option ref
      }
  | RECinfo of record_kind * (value * accesspath) list
  | SELinfo of int * value * cty
  | OFFinfo of int * value
  | WRPinfo of P.numkind * value				(* P.wrap of a value *)
  | IFIDIOMinfo of {body : (lvar * cexp * cexp) option ref}
  | MISCinfo of cty

fun contract {function=(fkind,fvar,fargs,ctyl,cexp), click, last, size=cpssize} =
(* NOTE: the "last" argument is currently ignored. *)
let

val deadup = !Control.CG.deadup
val CGbetacontract = !Control.CG.betacontract
val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say(s) else ()
fun debugflush() = if debug then Control.Print.flush() else ()

local exception UsageMap
in  val m : {info: info, used : int ref, called : int ref}
		IntHashTable.hash_table =
	        IntHashTable.mkTable(128, UsageMap)
    val get = fn i => IntHashTable.lookup m i
	        handle UsageMap => bug ("UsageMap on " ^ Int.toString i)
    val enter = IntHashTable.insert m
    fun rmv i = ignore (IntHashTable.remove m i) handle _ => ()
end

fun use(VAR v) = inc(#used(get v))
  | use(LABEL v) = inc(#used(get v))
  | use _ = ()
fun use_less(VAR v) = if deadup then dec(#used(get v)) else ()
  | use_less(LABEL v) = if deadup then dec(#used(get v)) else ()
  | use_less _ = ()
fun usedOnce v = !(#used(get v)) = 1
fun used v = !(#used(get v)) > 0

fun call(VAR v) =
    let val {called,used,...} = get v
    in  inc called; inc used
    end
  | call(LABEL v) = call(VAR v)
  | call _ = ()
fun call_less(VAR v) = if deadup then
                         let val {called,used,...} = get v
			 in  dec called; dec used
			 end
		       else ()
  | call_less(LABEL v) = call_less(VAR v)
  | call_less _ = ()
fun call_and_clobber(VAR v) =
    let val {called,used,info} = get v
    in  inc called; inc used;
	case info
	  of FNinfo{body,...} => body := NONE
	   | _ => ()
    end
  | call_and_clobber(LABEL v) = call(VAR v)
  | call_and_clobber _ = ()

fun enterREC(w,kind,vl) = enter(w,{info=RECinfo(kind,vl), called=ref 0,used=ref 0})
fun enterMISC (w,ct) = enter(w,{info=MISCinfo ct, called=ref 0, used=ref 0})
val miscBOG = MISCinfo CPSUtil.BOGt
fun enterMISC0 w = enter(w,{info=miscBOG, called=ref 0, used=ref 0})
fun enterWRP (w, kind, u) = enter(w, {info=WRPinfo(kind, u), called=ref 0, used=ref 0})

fun enterFN (_,f,vl,cl,cexp) =
      (enter(f,{called=ref 0,used=ref 0,
		info=FNinfo{args=vl,
			    body=ref(if CGbetacontract then SOME cexp
				     else NONE),
			    specialuse=ref NONE,
			    liveargs=ref NONE}});
       ListPair.appEq enterMISC (vl, cl))

(*********************************************************************
   checkFunction: used by pass1(FIX ...) to decide
   (1) whether a function will be inlined for the if idiom;
   (2) whether a function will drop some arguments.
 *********************************************************************)
fun checkFunction(_,f,vl,_,_) =
 (case get f
    of {called=ref 2,used=ref 2,
	info=FNinfo{specialuse=ref(SOME(ref 1)),
		    body as ref(SOME(BRANCH(_,_,c,a,b))),...},...} =>
	   if not (!CG.ifidiom) then body:=NONE
	   else (* NOTE: remapping f *)
	        enter(f,{info=IFIDIOMinfo{body=ref(SOME(c,a,b))},
			 called=ref 2, used=ref 2})
     | {called=ref c,used=ref u,info=FNinfo{liveargs,...}} =>
	   if u<>c (* escaping function *)
	       orelse not(!CG.dropargs) then ()
	   else liveargs := SOME(map used vl)
     | _  => ())


(**************************************************************************)
(* pass1: gather usage information on the variables in a cps expression,  *)
(* and make a few decisions about whether to inline functions:            *)
(*        (1) If Idiom                                                    *)
(*        (2) NO_INLINE_INTO                                              *)
(**************************************************************************)
val rec pass1 = fn cexp => p1 false cexp
and p1 = fn no_inline =>
let val rec g1 =
 fn RECORD(kind,vl,w,e) => (enterREC(w,kind,vl); app (use o #1) vl; g1 e)
  | SELECT (i,v,w,ct,e) =>
      (enter(w,{info=SELinfo(i,v,ct), called=ref 0, used=ref 0});
       use v; g1 e)
  | OFFSET (i,v,w,e) =>
      (enter(w,{info=OFFinfo(i,v), called=ref 0, used=ref 0});
       use v; g1 e)
  | APP(f, vl) => (if no_inline
		       then call_and_clobber f
		   else call f;
		   app use vl)
  | FIX(l, e) => (app enterFN l;
		  app (fn (NO_INLINE_INTO,_,_,_,body) => p1 (not last) body
		        | (_,_,_,_,body) => g1 body) l;
		  g1 e;
		  app checkFunction l)
  | SWITCH(v,c,el) => (use v; enterMISC0 c; app g1 el)
  | BRANCH(i,vl,c,e1 as APP(VAR f1, [NUM{ival = 1, ...}]),
		  e2 as APP(VAR f2, [NUM{ival = 0, ...}])) =>
       (case get f1
	 of {info=FNinfo{body=ref(SOME(BRANCH(P.cmp{oper=P.NEQ,...},[NUM{ival = 0, ...}, VAR w2],_,_,_))),
			 args=[w1],specialuse,...},...} =>
              (* Handle IF IDIOM *)
    	      if f1=f2 andalso w1=w2
	      then let val {used,...}=get w1
		   in  specialuse := SOME used
		   end
	      else ()
	  | _ => ();
	app use vl; enterMISC(c,CPSUtil.BOGt); g1 e1; g1 e2)
  | BRANCH(i,vl,c,e1,e2) => (app use vl; enterMISC0 c; g1 e1; g1 e2)
  | SETTER(i,vl,e) => (app use vl; g1 e)
  | LOOKER(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | ARITH(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | PURE(P.wrap kind, [u], w, _, e) => (use u; enterWRP(w, kind, u); g1 e)
  | PURE(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | RCC(k,l,p,vl,wtl,e) => (app use vl; app (enterMISC0 o #1) wtl; g1 e)
in  g1
end


local
   exception Beta
   val m2 : value IntHashTable.hash_table = IntHashTable.mkTable(32, Beta)
   val mapm2 = IntHashTable.lookup m2
in

fun ren(v0 as VAR v) = (ren(mapm2 v) handle Beta => v0)
  | ren(v0 as LABEL v) = (ren(mapm2 v) handle Beta => v0)
  | ren x = x

fun newname (vw as (v,w)) =
 let val {used=ref u,called=ref c,...} = get v
     fun f(VAR w') = let val {used,called,...} = get w'
	             in  used := !used + u; called := !called + c
		     end
       | f(LABEL w') = f(VAR w')
       | f _ = ()
 in  if deadup then f (ren w) else ();
     rmv v;
     sameName vw; IntHashTable.insert m2 vw
 end

end (* local *)

fun newnames(v::vl, w::wl) = (newname(v,w); newnames(vl,wl))
  | newnames _ = ()


(*********************************************************************)
(* drop_body: used when dropping a function to adjust the usage      *)
(* counts of the free variables of the function.                     *)
(* This should match up closely with pass1 above.                    *)
(*********************************************************************)
local val use_less = use_less o ren
      val call_less = call_less o ren
in
fun drop_body(APP(f,vl)) = (call_less f; app use_less vl)
  | drop_body(SELECT(_,v,_,_,e)) = (use_less v; drop_body e)
  | drop_body(OFFSET(_,v,_,e)) = (use_less v; drop_body e)
  | drop_body(RECORD(_,vl,_,e)) = (app (use_less o #1) vl; drop_body e)
  | drop_body(FIX(l,e)) = (app (drop_body o #5) l; drop_body e)
  | drop_body(SWITCH(v,_,el)) = (use_less v; app drop_body el)
  | drop_body(BRANCH(_,vl,_,e1,e2)) = (app use_less vl;
				       drop_body e1; drop_body e2)
  | drop_body(SETTER(_,vl,e)) = (app use_less vl; drop_body e)
  | drop_body(LOOKER(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body(ARITH(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body(PURE(_,vl,_,_,e)) = (app use_less vl; drop_body e)
  | drop_body(RCC(_,_,_,vl,_,e)) = (app use_less vl; drop_body e)
end (* local *)

fun setter (P.update, [_, _, NUM{ty={tag=true, ...}, ...}]) = P.unboxedupdate
  | setter (P.update, _) = P.update
  | setter (P.assign, [_, NUM{ty={tag=true, ...}, ...}]) = P.unboxedassign
  | setter (i, _) = i

fun sameLvar(lvar, VAR lv) = lv = lvar
  | sameLvar _ = false

fun cvtPreCondition(n:int, n2, x, v2) =
  n = n2 andalso usedOnce(x) andalso sameLvar(x, ren v2)
fun cvtPreCondition_inf(x, v2) =
  usedOnce(x) andalso sameLvar(x, ren v2)

val rec reduce = fn cexp => g NONE cexp
and g = fn hdlr =>
let val rec g' =
  fn RECORD (kind,vl,w,e) =>
      let val {used,...} = get w
	  val vl' = map (map1 ren) vl
       in if !used=0 andalso !CG.deadvars
	  then (click "b"; app (use_less o #1) vl'; g' e)
          else let
	  (* Check to see if this record is recreating an existing record.
	   * We need to be careful that the existing record has the same
	   * kind as this record (as well as the same size and content).
	   *)
	    fun objInfo (VAR z) =(case (#info (get z))
		   of SELinfo(_,_,PTRt(RPT k)) => (SOME RK_RECORD, k)
		    | SELinfo(_,_,PTRt(FPT k)) => (NONE, k)
		    | MISCinfo(PTRt(RPT k)) => (SOME RK_RECORD, k)
		    | MISCinfo(PTRt(FPT k)) => (NONE, k)
		    | RECinfo(kind, l) => (SOME kind, length l)
		    | _ => (NONE, ~1))
	      | objInfo _ = (NONE, ~1)

	    fun samevar(VAR x,VAR y) = (x=y)
	      | samevar _ = false

	    fun check1((VAR z)::r,j,a) =
		  (case (get z)
		    of {info=SELinfo(i,b,_),...} =>
			   (if ((i=j) andalso (samevar(ren b,a)))
			    then check1(r,j+1,a) else NONE)
		     | _ => NONE)
	      | check1(_::r,j,_) = NONE
	      | check1([],j,a) = (case objInfo a
		   of (SOME kind', n) => if (kind = kind') andalso (n = j)
			then SOME a
			else NONE
		    | (NONE, _) => NONE
		  (* end case *))

	    fun check((VAR z)::r) =
		  (case (get z)
		    of {info=SELinfo(0,a,_),...} =>
			  check1(r,1,ren a)
		     | _ => NONE)
	      | check _ = NONE

	    val vl'' = map #1 vl'

	     in case (check(vl''))
		 of NONE =>
		     (let val e' = g' e
		       in if !used=0 andalso deadup
			  then (click "B1"; app use_less vl''; e')
			  else RECORD(kind, vl', w, e')
		      end)
		  | SOME z =>
		     (newname(w,z); click "B2"; (*** ? ***)
		      app use_less vl''; g' e)
	    end
      end
   | SELECT(i,v,w,t,e) =>
      let val {used,...} = get w
          val v' = ren v
      in  if !used=0 andalso !CG.deadvars
	      then (click "c"; (* could rmv w here *)
		    use_less v';
		    g' e)
	  else let val z = (case v'
			      of VAR v'' =>
				  (case get v''
				     of {info=RECinfo(_, vl),...} =>
					 (let val z = #1(List.nth(vl,i))
					      val z' = ren z
					  in
                                             case z'
                                               of REAL _ => NONE
                                                | _  => SOME z'
					  end handle Subscript => NONE)
				      | _ => NONE)
			       | _ => NONE)
		   val z = if !CG.selectopt then z else NONE
	       in  case z
		     of NONE => let val e' = g' e
				in  if !used=0 andalso deadup
					then (click "s";
					      use_less v';
					      e')
				    else SELECT(i,v',w,t,e')
				end
		      | SOME z' => (newname(w,z');
				    click "d"; (* could rmv w here *)
				    use_less v';
				    g' e)
	       end
      end
   | OFFSET(i,v,w,e) => OFFSET(i,ren v,w,g' e)
   | APP(f, vl) =>
      let val vl' = map ren vl
	  val f' = ren f
	  fun newvl NONE = vl'
	    | newvl (SOME live) =
	      let fun z(a::al,false::bl) = z(al,bl)
		    | z(a::al,true::bl) = a::z(al,bl)
		    | z _ = nil
	      in  (* This code may be obsolete.  See the comment
		     in the FIX case below. *)
		  case z(vl',live)
		    of nil => [tagInt 0]
		     | vl'' => vl''
	      end
	  fun trybeta fv =
	    let val {used=ref u,called=ref c,info} = get fv
	    in  case info
		  of FNinfo{args,body,liveargs,...} =>
		      if c<>1 orelse u<>1 then APP(f',newvl(!liveargs))
		      else (case body
			      of ref(SOME b) =>
				  (newnames(args, vl');
				   call_less f';
				   app use_less vl';
				   body:=NONE;
				   g' b)
			       | _ => APP(f',newvl(!liveargs)))
		   | _ => APP(f',vl')
	    end
      in  case f'
	    of VAR fv => trybeta fv
	     | LABEL fv => trybeta fv
	     | _ => APP(f',vl')
      end
   | FIX(l,e) =>
      let fun getinfo (x as (fk,f,vl,cl,b)) =
	    let val {used,called,info,...} = get f
	    in  case info
		  of FNinfo{liveargs=ref(SOME live),...} =>
		      let fun z(a::al,false::bl) = z(al,bl)
			    | z(a::al,true::bl) = a::z(al,bl)
			    | z _ = nil
			  val vl' = z(vl,live)
			  val cl' = z(cl,live)
			  val drop =
			      foldr (fn (a,b) => if a then b else b+1) 0 live
			  fun dropclicks(n) =
			      if n > 0 then (click "D"; dropclicks(n-1))
			      else ()
			  val (vl'', cl'') =
			      case vl'
			       of nil => let val x = LV.mkLvar()
				   in  dropclicks(drop - 1);
				       enterMISC0 x;
				       ([x], [tagIntTy])
				   end
 			        | _ => (dropclicks(drop); (vl',cl'))

		      in
			  ((fk,f,vl'',cl'',b),used,called,info)
		      end
		   | _ => (x,used,called,info)
	    end
	  fun keep (_,used,called,info) =
               (case (!called,!used,info)
		  of (_,0,FNinfo{body as ref(SOME b),...}) =>
			 (click "g";
			  body:=NONE;
			  drop_body b;
			  false)
		   | (_,0,FNinfo{body=ref NONE,...}) =>
			 (click "g"; false)
		   | (1,1,FNinfo{body=ref(SOME _),...}) =>
			 (* NOTE: this is an optimistic click.
                            The call could disappear before we
			    get there; then the body would
			    not be cleared out, dangerous. *)
			 (click "e"; false)
		   | (_,_,IFIDIOMinfo{body=ref b,...}) =>
			 (click "E"; false)
		   | _ => true)
	  fun keep2 (_,used,_,info) =
               (case (!used,info)
		  of (0,FNinfo{body as ref(SOME b),...}) =>
		         (* All occurrences were lost *)
			 (click "f";
			  body:=NONE;
			  drop_body b;
			  false)
		   | (0,FNinfo{body=ref NONE,...}) =>
			 (* We performed a cascaded inlining *)
			 (click "q"; false)
		   | (_,FNinfo{body,...}) => (body:=NONE; true)
		   | _ => true)
	  fun keep3 ((_,_,_,_,b),used,_,info) =
               (case (!used,info)
		  of (0,FNinfo _) =>
		         (* All occurrences were lost *)
			 (click "f";
			  drop_body b;
			  false)
		   | _ => true)
	  fun reduce_body ((fk,f,vl,cl,body),used,called,info) =
	         ((fk,f,vl,cl,reduce body),used,called,info)
	  val l1 = map getinfo l
	  val l2 = List.filter keep l1
	  val e' = g' e
	  val l3 = List.filter keep2 l2
	  val l4 = map reduce_body l3
      in  case (List.filter keep3 l4)
	    of nil => e'
	     | l5 => FIX(map #1 l5, e')
      end
   | SWITCH(v,c,el) => (case ren v
	 of v' as NUM{ival, ty={tag=true, ...}} => if !CG.switchopt
	     then let
	       val i = IntInf.toInt ival
	       fun f (e::el, j) = (if i=j then () else drop_body e; f(el, j+1))
		 | f ([], _) = ()
	       in
		 click "h";
		 f(el, 0);
		 newname(c, tagInt 0);
		 g' (List.nth(el,i))
	       end
	     else SWITCH(v', c, map g' el)
	  | v' => SWITCH(v',c, map g' el)
	(* end case *))
   | LOOKER(P.gethdlr,_,w,t,e) =>
      (if !CG.handlerfold
       then case hdlr
             of NONE => if used w
                        then LOOKER(P.gethdlr,[],w,t,g (SOME(VAR w)) e)
		        else (click "i"; g' e)
              | SOME w' => (click "j"; newname(w,w'); g' e)
       else LOOKER(P.gethdlr,[],w,t,g (SOME(VAR w)) e))
   | SETTER(P.sethdlr,[v],e) =>
      let val v' = ren v
	  val e' = g (SOME v') e
	  fun sameVar (VAR x, VAR y) = x = y
	    | sameVar _ = false
      in  if !CG.handlerfold
	  then case hdlr
		 of SOME v'' =>
		     if sameVar (v', v'') then (click "k"; use_less v''; e')
		     else SETTER(P.sethdlr,[v'],e')
		  | _ => SETTER(P.sethdlr,[v'],e')
	  else SETTER(P.sethdlr,[v'],e')
      end
(* | SETTER(i,vl,e) => SETTER(i, map ren vl, g' e) *)
   | SETTER(i,vl,e) =>
      let val vl' = map ren vl
      in  SETTER(setter (i, vl'), vl', g' e)
      end
   | LOOKER(i,vl,w,t,e) =>
      let val vl' = map ren vl
	  val {used,...} = get w
      in  if !used=0 andalso !CG.deadvars
	      then (click "m"; app use_less vl'; g' e)
	  else let val e' = g' e
	       in  if !used=0 andalso deadup
		   then (click "*"; app use_less vl'; e')
		   else LOOKER(i, vl', w, t, e')
	       end
      end
   | ARITH(P.test(p,n),[v],x,t,e as PURE(P.copy(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) andalso n = m then
	(click "T(1)"; ARITH(P.test(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.test(p,n), [ren v], x, t, g' e)
   | ARITH(P.test_inf n,[v,f],x,t,e as PURE(P.copy(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) andalso n = m then
	(click "T(1)"; ARITH(P.test_inf m, [ren v, ren f], x2, t2, g' e2))
      else ARITH(P.test_inf n, [ren v, ren f], x, t, g' e)
   | ARITH(P.test(p,n),[v],x,t,e as ARITH(P.test(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) then
	(click "T(2)"; ARITH(P.test(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.test(p,n), [ren v], x, t, g' e)
   | ARITH(P.test_inf n,[v, f],x,t,e as ARITH(P.test(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) then
	(click "T(2)"; ARITH(P.test_inf m, [ren v, ren f], x2, t2, g' e2))
      else ARITH(P.test_inf n, [ren v, ren f], x, t, g' e)
   | ARITH(P.testu(p,n),[v],x,t,e as PURE(P.copy(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) andalso n = m then
	(click "U(1)"; ARITH(P.testu(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.testu(p,n), [ren v], x, t, g' e)
   | ARITH(P.testu(p,n),[v],x,t,e as ARITH(P.testu(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) then
	(click "U(2)"; ARITH(P.testu(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.testu(p,n), [ren v], x, t, g' e)

   | ARITH(i,vl,w,t,e) =>
      let val vl' = map ren vl
      in  (if !CG.arithopt
	   then (newname(w,arith(i, vl')); app use_less vl'; g' e)
	   else raise ConstFold)
	  handle ConstFold => ARITH(i, vl', w, t, g' e)
	       | Overflow => ARITH(i, vl', w, t, g' e)
      end

   | PURE(P.trunc(p,n), [v], x, t, e as PURE(pure, [v2], x2, t2, e2)) => let
      fun skip() = PURE(P.trunc(p,n), [ren v], x, t, g' e)
      fun checkClicked(tok, n2, m, pureOp) =
	if cvtPreCondition(n, n2, x, v2) then
	  (click tok;
	   PURE(pureOp(p,m), [ren v], x2, t2, g' e2))
	else skip()
     in
       case pure
	of P.trunc(n2,m) => checkClicked("R(1)", n2, m, P.trunc)
         | P.copy(n2,m) =>
	    if n2=m then checkClicked("R(2)", n2, m, P.trunc) else skip()
	 | _  => skip()
     end
   | PURE(P.trunc_inf n, [v, f], x, t,
	  e as PURE(pure, [v2], x2, t2, e2)) => let
      fun skip() = PURE(P.trunc_inf n, [ren v, ren f], x, t, g' e)
      fun checkClicked(tok, n2, m) =
	if cvtPreCondition(n, n2, x, v2) then
	  (click tok;
	   PURE(P.trunc_inf m, [ren v, ren f], x2, t2, g' e2))
	else skip()
     in
       case pure
	of P.trunc(n2,m) => checkClicked("R(1)", n2, m)
         | P.copy(n2,m) =>
	    if n2=m then checkClicked("R(2)", n2, m) else skip()
	 | _  => skip()
     end
   | PURE(P.extend(p,n), [v], x, t,
	  e as PURE(P.extend_inf n2, [v2,f], x2, t2, e2)) =>
	 if cvtPreCondition(n,n2,x,v2) then
	     (click "X(1')";
	      PURE(P.extend_inf p, [ren v, ren f], x2, t2, g' e2))
	 else
	     PURE(P.extend(p,n), [ren v], x, t, g' e)
   | PURE(P.extend(p,n), [v], x, t, e as PURE(pure, [v2], x2, t2, e2)) => let
       fun skip() = PURE(P.extend(p,n), [ren v], x, t, g' e)
       fun checkClicked(tok, n2, pureOp) =
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok;
	    PURE(pureOp, [ren v], x2, t2, g' e2))
	 else skip()
     in
       case pure
	of P.extend(n2,m) => checkClicked("X(1)", n2, P.extend (p, m))
         | P.copy(n2,m) =>
	    if n2 = m then checkClicked("X(2)", n2, P.extend (p, m))
	    else skip()
	 | P.trunc(n2,m) =>
	    if m >= p then checkClicked("X(3)", n2, P.extend (p, m))
	    else checkClicked("X(4)", n2, P.trunc (p, m))
	 | _ => skip()
     end
   | PURE(P.extend_inf p, [v,f], x, t,
	  e as PURE(P.trunc_inf m, [v2, f2], x2, t2, e2)) =>
     let fun checkClicked(tok, pureOp) =
	     if cvtPreCondition_inf(x, v2) then
		 (click tok;
		  use_less f; use_less f2;
		  PURE(pureOp, [ren v], x2, t2, g' e2))
	     else PURE (P.extend_inf p, [ren v, ren f], x, t, g' e)
     in
	 if m >= p then checkClicked("X(3')", P.extend(p, m))
	 else checkClicked("X(4')", P.trunc(p, m))
     end
   | PURE(P.extend(p,n), [v], x, t, e as ARITH(a, [v2], x2, t2, e2)) => let
       val v' = [ren v]
       fun skip() = PURE(P.extend(p,n), v', x, t, g' e)
       fun checkClicked(tok, n2, m, arithOp) =
	 if cvtPreCondition(n, n2, x, v2) then
	   if m >= p then
	     (click tok; PURE(P.extend(p,m), v', x2, t2, g' e2))
	   else ARITH(arithOp(p,m), v', x2, t2, g' e2)
	 else skip()
     in
       case a
	of P.test(n2, m) => checkClicked("X(5)", n2, m, P.test)
         | P.testu(n2, m) => checkClicked("X(6)", n2, m, P.testu)
	 | _ => skip()
     end
   | PURE(P.extend_inf p, [v, f], x, t,
	  e as ARITH (P.test_inf m, [v2, f2], x2, t2, e2)) =>
       if cvtPreCondition_inf (x, v2) then
	   if m >= p then
	       (click "X9"; use_less f; use_less f2;
		PURE (P.extend (p, m), [ren v], x2, t2, g' e2))
	   else ARITH (P.test (p, m), [ren v], x2, t2, g' e2)
       else PURE (P.extend_inf p, [ren v, ren f], x, t, g' e)
   | PURE (P.copy (p, n), [v], x, t,
	   e as PURE (P.copy_inf n2, [v2, f2], x2, t2, e2)) =>
       if cvtPreCondition (n, n2, x, v2) then
	   (click "C(2)";
	    PURE (P.copy_inf p, [ren v, ren f2], x2, t2, g' e2))
       else
	   PURE (P.copy (p, n), [ren v], x, t, g' e)
   | PURE (P.copy (p, n), [v], x, t,
	   e as PURE (P.extend_inf n2, [v2, f2], x2, t2, e2)) => let
	 fun skip () = PURE (P.copy (p, n), [ren v], x, t, g' e)
	 fun checkClicked(tok, pureOp) =
	     if cvtPreCondition (n, n2, x, v2) then
		 (click tok; PURE (pureOp, [ren v, ren f2], x2, t2, g' e2))
	     else skip ()
     in
	 if n > p then checkClicked("C(2')", P.copy_inf p)
	 else if n = p then checkClicked("C(2')", P.extend_inf p)
	 else skip ()
     end
   | PURE(P.copy(p,n), [v], x, t, e as PURE(pure, [v2], x2, t2, e2)) => let
       val v' = [ren v]
       fun skip () = PURE(P.copy(p,n), v', x, t, g' e)
       fun checkClicked(tok, n2, pureOp) =
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok; PURE(pureOp, v', x2, t2, g' e2))
	 else skip()
     in
       case pure
	of P.copy(n2,m) => checkClicked("C(1)", n2, P.copy (p, m))
         | P.extend(n2,m) =>
	    if n > p then checkClicked("C(2)", n2, P.copy (p, m))
	    else if n = p then checkClicked("C(2)", n2, P.extend (p, m))
	    else skip()
   	 | P.trunc(n2,m) =>
            if m >= p then checkClicked("C(3)", n2, P.copy (p, m))
	    else if m < p then checkClicked("C(4)", n2, P.trunc (p, m))
	    else skip()
	 | _ => skip()
     end
   | PURE (P.copy_inf p, [v, f], x, t,
	   e as PURE (P.trunc_inf m, [v2, f2], x2, t2, e2)) => let
	 fun skip () = PURE (P.copy_inf p, [ren v, ren f], x, t, g' e)
	 fun checkClicked (tok, pureOp) =
	     if cvtPreCondition_inf (x, v2) then
		(click tok;
		 use_less f; use_less f2;
		 PURE (pureOp, [ren v], x2, t2, g' e2))
	     else skip ()
     in
	 if m >= p then checkClicked ("C(3)", P.copy (p, m))
	 else if m < p then checkClicked ("C(4)", P.trunc (p, m))
	 else skip ()
     end
   | PURE(P.copy(p,n), [v], x, t, e as ARITH(a, [v2], x2, t2, e2)) => let
       val v' = [ren v]
       fun skip () = PURE(P.copy(p,n), v', x, t, g' e)
       fun checkClicked(tok, n2, class, arithOp) =
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok; class(arithOp, v', x2, t2, g' e2))
	 else skip()
     in
       case a
	of P.test(n2,m) =>
	   if m >= p then checkClicked("C5", n2, PURE, P.copy (p, m))
	   else checkClicked("C6", n2, ARITH, P.test (p, m))
	 | P.testu(n2,m) =>
	   if m > p then checkClicked("C7", n2, PURE, P.copy (p, m))
	   else checkClicked("C8", n2, ARITH, P.testu (p, m))
	 | _ => skip()
     end
   | PURE (P.copy_inf p, [v, f], x, t,
	   e as ARITH (P.test_inf m, [v2, f2], x2, t2, e2)) => let
	 fun checkClicked (tok, class, oper) =
	     if cvtPreCondition_inf (x, v2) then
		 (click tok; use_less f; use_less f2;
		  class (oper, [ren v], x2, t2, g' e2))
	     else PURE (P.copy_inf p, [ren v, ren f], x, t, g' e)
     in
	 if m >= p then checkClicked ("C5", PURE, P.copy (p, m))
	 else checkClicked ("C6", ARITH, P.test (p, m))
     end
   | PURE(i,vl,w,t,e) =>
      let val vl' = map ren vl
	  val {used,...} = get w
      in  if !used=0 andalso !CG.deadvars
	  then (click "m"; app use_less vl'; g' e)
	  else ((if !CG.arithopt
	         then (newname(w,pure(i, vl')); g' e)
	         else raise ConstFold)
	        handle ConstFold =>
		    let val e' = g' e
		    in  if !used=0 andalso deadup
			then (app use_less vl'; click "*"; e')
			else PURE(i, vl', w, t, e')
		    end)
      end
   | RCC(k,l,p,vl,wtl,e) =>
     (* leave raw C calls alone *)
       RCC (k, l, p, map ren vl, wtl, g' e)
   | BRANCH(i,vl,c,e1,e2) =>
      let val vl' = map ren vl
	  fun h() = (if !CG.branchfold andalso equalUptoAlpha(e1,e2)
		     then (click "z";
			   app use_less vl';
			   newname(c,tagInt 0);
			   drop_body e2;
			   g' e1)
		     else if !CG.comparefold
		     then if branch(i,vl')
			       then (newname(c,tagInt 0);
				     app use_less vl';
				     drop_body e2;
				     g' e1)
			       else (newname(c,tagInt 0);
				     app use_less vl';
				     drop_body e1;
				     g' e2)
		     else raise ConstFold)
		 handle ConstFold => BRANCH(i, vl', c, g' e1, g' e2)
	  fun getifidiom f =
	    let val f' = ren f
	    in  case f'
		  of VAR v =>
		      (case get v
			 of {info=IFIDIOMinfo{body},...} => SOME body
			  | _ => NONE)
		   | _ => NONE
	    end
      in  case (e1,e2)
           of (APP(VAR f, [NUM{ival=1, ...}]), APP(VAR f', [NUM{ival=0, ...}])) =>
	       (case (f=f', getifidiom(VAR f))
                  of (true,
	              SOME(body as ref(SOME(c',a,b)))) =>
		        (* Handle IF IDIOM *)
		        (newname(c', VAR c);
			 body:=NONE;
			 (* NOTE: could use vl' here instead of vl. *)
			 g'(BRANCH(i,vl,c,a,b)))
	           | _ => h())
	    | _ => h()
      end
in  g'
end

(* statically evaluate a boolean test; either return the result or raise ConstFold *)
 and branch =
    fn (P.unboxed, vl) => not(branch(P.boxed, vl))
     | (P.boxed, [NUM{ty={tag, ...}, ...}]) => (click "n"; not tag)
     | (P.boxed, [STRING s]) => (click "o"; true)
     | (P.boxed, [VAR v]) => (case get v
	 of {info=RECinfo _, ...} => (click "p"; true)
	  | _ => raise ConstFold)
     | (P.cmp{oper=P.LT, ...}, [VAR v, VAR w]) =>
	  if v=w then (click "v"; false) else raise ConstFold
     | (P.cmp{oper=P.LTE, ...}, [VAR v, VAR w]) =>
	  if v=w then (click "v"; true) else raise ConstFold
     | (P.cmp{oper=P.LT, kind=P.INT _}, [NUM i, NUM j]) => (
	  click "w"; #ival i < #ival j)
     | (P.cmp{oper=P.LT, kind=P.UINT sz}, [NUM i, NUM j]) => (
	  click "w"; CA.uLess(sz, #ival i, #ival j))
     | (P.cmp{oper=P.LTE, kind=P.INT _}, [NUM i, NUM j]) => (
	  click "w"; #ival i <= #ival j)
     | (P.cmp{oper=P.LTE, kind=P.UINT sz}, [NUM i, NUM j]) => (
	  click "w"; CA.uLessEq(sz, #ival i, #ival j))
     | (P.cmp{oper=P.GT, kind}, [w,v]) =>
	  branch(P.cmp{oper=P.LT, kind=kind}, [v,w])
     | (P.cmp{oper=P.GTE, kind}, vl) =>
	  not (branch(P.cmp{oper=P.LT, kind=kind}, vl))
     | (P.cmp{oper=P.EQL, kind=P.FLOAT _}, _) => raise ConstFold (* in case of NaN's *)
     | (P.cmp{oper=P.EQL, ...}, [VAR v, VAR w]) =>
	  if v=w then  (click "v"; true) else raise ConstFold
     | (P.cmp{oper=P.EQL, ...}, [NUM i, NUM j]) => (click "w"; #ival i = #ival j)
     | (P.cmp{oper=P.NEQ, kind}, vl) =>
	  not(branch(P.cmp{oper=P.EQL, kind=kind}, vl))
     | (P.peql, [NUM i, NUM j]) => (click "w"; #ival i = #ival j)
     | (P.pneq, vl) => not(branch(P.peql, vl))
     | _ => raise ConstFold

  and arith =
    fn (P.arith{oper=P.MUL, ...}, [NUM{ival=1, ...}, v]) => (click "F"; v)
     | (P.arith{oper=P.MUL, ...}, [v, NUM{ival=1, ...}]) => (click "G"; v)
     | (P.arith{oper=P.MUL, ...}, [v as NUM{ival=0, ...}, _]) => (click "H"; v)
     | (P.arith{oper=P.MUL, ...}, [_, v as NUM{ival=0, ...}]) => (click "I"; v)
     | (P.arith{oper=P.MUL, kind=P.INT sz}, [NUM i, NUM j]) => let
	  val x = CA.sMul(sz, #ival i, #ival j)
	  in
	    click "J"; NUM{ival = x, ty = #ty i}
	  end
     | (P.arith{oper=P.QUOT, ...}, [v, NUM{ival=1, ...}]) => (click "K"; v)
     | (P.arith{oper=P.QUOT, ...}, [_, NUM{ival=0, ...}]) => raise ConstFold
     | (P.arith{oper=P.QUOT, kind=P.INT sz}, [NUM i, NUM j]) => let
	  val x = CA.sQuot(sz, #ival i, #ival j)
	  in
	    click "L"; NUM{ival = x, ty = #ty i}
	  end
     | (P.arith{oper=P.DIV, ...}, [v, NUM{ival=1, ...}]) => (click "K"; v)
     | (P.arith{oper=P.DIV, ...}, [_, NUM{ival=0, ...}]) => raise ConstFold
     | (P.arith{oper=P.DIV, kind=P.INT sz}, [NUM i, NUM j]) => let
	  val x = CA.sDiv(sz, #ival i, #ival j)
	  in
	    click "L"; NUM{ival = x, ty = #ty i}
	  end
     (* FIXME: should we do anything for mod or rem here? *)
     | (P.arith{oper=P.ADD, ...}, [NUM{ival=0, ...}, v]) => (click "M"; v)
     | (P.arith{oper=P.ADD, ...}, [v, NUM{ival=0, ...}]) => (click "N"; v)
     | (P.arith{oper=P.ADD, kind=P.INT sz}, [NUM i, NUM j]) => let
	  val x = CA.sAdd(sz, #ival i, #ival j)
	  in
	    click "O"; NUM{ival = x, ty = #ty i}
	  end
     | (P.arith{oper=P.SUB, ...}, [v, NUM{ival=0, ...}]) => (click "P"; v)
     | (P.arith{oper=P.SUB, kind=P.INT sz}, [NUM i, NUM j]) => let
	  val x = CA.sSub(sz, #ival i, #ival j)
	  in
	    click "Q"; NUM{ival = x, ty = #ty i}
	  end
     | (P.arith{oper=P.NEG, kind=P.INT sz}, [NUM i]) => let
	  val x = CA.sNeg(sz, #ival i)
	  in
	    click "X"; NUM{ival = x, ty = #ty i}
	  end
     | _ => raise ConstFold

(* pure arithmetic operations; raises ConstFold when there is no reduction *)
  and pure =
    fn (P.pure_arith{oper=P.MUL, ...}, [NUM{ival=1, ...}, v]) => (click "F"; v)
     | (P.pure_arith{oper=P.MUL, ...}, [v, NUM{ival=1, ...}]) => (click "G"; v)
     | (P.pure_arith{oper=P.MUL, ...}, [v as NUM{ival=0, ...}, _]) => (click "H"; v)
     | (P.pure_arith{oper=P.MUL, ...}, [_, v as NUM{ival=0, ...}]) => (click "I"; v)
(* FIXME: 32-bit dependent code *)
     | (P.pure_arith{oper=P.MUL, kind=P.UINT sz}, [NUM i, NUM j]) => let
          val x = CA.uMul(sz, #ival i, #ival j)
	  in
	    click "J"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.ADD, ...}, [NUM{ival=0, ...}, v]) => (click "M"; v)
     | (P.pure_arith{oper=P.ADD, ...}, [v, NUM{ival=0, ...}]) => (click "N"; v)
     | (P.pure_arith{oper=P.ADD, kind=P.UINT sz}, [NUM i, NUM j]) => let
	  val x = CA.uAdd(sz, #ival i, #ival j)
	  in
	    click "O"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.SUB, ...}, [v, NUM{ival=0, ...}]) => (click "P"; v)
     | (P.pure_arith{oper=P.SUB, kind=P.UINT sz}, [NUM i, NUM j]) => let
	  val x = CA.uSub(sz, #ival i, #ival j)
	  in
	    click "Q"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.RSHIFT, ...}, [i as NUM{ival=0, ...}, _]) => (click "S"; i)
     | (P.pure_arith{oper=P.RSHIFT, ...}, [v, NUM{ival=0, ...}]) => (click "T"; v)
     | (P.pure_arith{oper=P.RSHIFT, kind}, [NUM i, NUM j]) => let
	  val x = CA.sShR(sizeOfKind kind, #ival i, #ival j)
	  in
	    click "R"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.RSHIFTL, ...}, [i as NUM{ival=0, ...}, _]) => (click "S"; i)
     | (P.pure_arith{oper=P.RSHIFTL, ...}, [v, NUM{ival=0, ...}]) => (click "T"; v)
     | (P.pure_arith{oper=P.RSHIFTL, kind=P.UINT sz}, [NUM i, NUM j]) => let
	  val x = CA.uShR(sz, #ival i, #ival j)
	  in
	    click "R"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.LSHIFT, ...}, [v as NUM{ival=0, ...}, _]) => (click "Z"; v)
     | (P.pure_arith{oper=P.LSHIFT, ...}, [v, NUM{ival=0, ...}]) => (click "1"; v)
     | (P.pure_arith{oper=P.LSHIFT, kind=P.INT sz}, [NUM i, NUM j]) => (let
	  val x = CA.sShL(sz, #ival i, #ival j)
	  in
	    click "Y"; NUM{ival = x, ty = #ty i}
	  end handle Overflow => raise ConstFold)
     | (P.pure_arith{oper=P.LSHIFT, kind=P.UINT sz}, [NUM i, NUM j]) => let
	  val x = CA.uShL(sz, #ival i, #ival j)
	  in
	    click "Y"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.ANDB, ...}, [v as NUM{ival=0, ...}, _]) => (click "0"; v)
     | (P.pure_arith{oper=P.ANDB, ...}, [_, v as NUM{ival=0, ...}]) => (click "T"; v)
     | (P.pure_arith{oper=P.ANDB, kind}, [NUM i, NUM j]) => let
	  val x = CA.bAnd(sizeOfKind kind, #ival i, #ival j)
	  in
	    click "9"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.ORB, ...}, [NUM{ival=0, ...}, v]) => (click "3"; v)
     | (P.pure_arith{oper=P.ORB, ...}, [v, NUM{ival=0, ...}]) => (click "4"; v)
     | (P.pure_arith{oper=P.ORB, kind}, [NUM i, NUM j]) => let
	  val x = CA.bOr(sizeOfKind kind, #ival i, #ival j)
	  in
	    click "2"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.XORB, ...}, [NUM{ival=0, ...}, v]) => (click "6"; v)
     | (P.pure_arith{oper=P.XORB, ...}, [v, NUM{ival=0, ...}]) => (click "7"; v)
     | (P.pure_arith{oper=P.XORB, kind}, [NUM i, NUM j]) => let
	  val x = CA.bXor(sizeOfKind kind, #ival i, #ival j)
	  in
	    click "5"; NUM{ival = x, ty = #ty i}
	  end
     | (P.pure_arith{oper=P.NOTB,kind}, [NUM i]) => let
	  val x = CA.bNot(sizeOfKind kind, #ival i)
	  in
	    click "8"; NUM{ival = x, ty = #ty i}
	  end
     | (P.length, [STRING s]) => (click "V"; tagInt'(size s))
     | (P.real{fromkind=P.INT _,tokind=P.FLOAT sz}, [NUM{ival, ...}]) =>
	(* NOTE: this conversion might lose precision *)
	  REAL{rval = RealLit.fromInt ival, ty=sz}
     | (P.unwrap(P.INT sz), [x as VAR v]) => (case get v
	   of {info=WRPinfo(P.INT sz', u), ...} => if (sz = sz')
		then (click "U"; use_less x; u)
		else bug "wrap/unwrap float size conflict"
	    | _ => raise ConstFold
	  (* end case *))
     | (P.unwrap(P.FLOAT sz), [x as VAR v]) => (case get v
	   of {info=WRPinfo(P.FLOAT sz', u), ...} => if (sz = sz')
		then (click "U"; use_less x; u)
		else bug "wrap/unwrap int size conflict"
	    | _ => raise ConstFold
	  (* end case *))
     | _ => raise ConstFold

in  debugprint "Contract: "; debugflush();
    enterMISC0 fvar; app enterMISC0 fargs;
    pass1 cexp;
    cpssize := IntHashTable.numItems m;
    let val cexp' = reduce cexp
    in  debugprint "\n";
	if debug
	    then (debugprint "After contract: \n";
		  PPCps.prcps cexp')
	else ();
	(fkind, fvar, fargs, ctyl, cexp')
    end
end

end (* functor Contract *)


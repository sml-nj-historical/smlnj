(* Copyright 1996 by Bell Laboratories *)
(* contract.sml *)

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

Dead variable elimination:         [down,up]           [down,up]
 RECORDs                              [b,B]        [deadvars,deadup]
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
                  table: LtyDef.lty IntHashTable.hash_table,
                  click: string -> unit,
                  last: bool,
                  size: int ref}
                  -> CPS.function
end (* signature CONTRACT *)

functor Contract(MachSpec : MACH_SPEC) : CONTRACT = 
struct

local

open CPS
structure LT = LtyExtern
structure LV = LambdaVar

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

val wtoi = Word.toIntX
val itow = Word.fromInt

structure CG = Control.CG

in

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Contract: " ^ s)

exception ConstFold

fun sublist pred nil = nil
  | sublist pred (hd::tl) = if (pred hd) then hd::(sublist pred tl)
			    else sublist pred tl

fun map1 f (a,b) = (f a, b)
fun app2(f,nil,nil) = ()
  | app2(f,a::al,b::bl) = (f(a,b);app2(f,al,bl))
  | app2(f,_,_) = bug "NContract app2 783"

fun sameName(x,VAR y) = LV.sameName(x,y) 
  | sameName(x,LABEL y) = LV.sameName(x,y) 
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
             app2(g,map LT.out l1, map LT.out l2)
        | g (LT.CONT t1,LT.CONT t2) = g(LT.out t1,LT.out t2) 
        | g (t1,t2) = complain(LT.inj t1, LT.inj t2,"CTR *** "^s)
  in  g(LT.out t1, LT.out t2) 
  end
*)

val isCont = LT.lt_iscont 

fun equalUptoAlpha(ce1,ce2) =
  let fun equ pairs =
        let fun same(VAR a, VAR b) = 
	          let fun look((x,y)::rest) = a=x andalso b=y orelse look rest
		        | look nil = false
		  in  a=b orelse look pairs
		  end
              | same(LABEL a, LABEL b) = same(VAR a, VAR b)
              | same(INT i, INT j) = i=j
              | same(REAL a, REAL b) = a=b
              | same(STRING a, STRING b) = a=b
	      | same(a,b) = false
            fun samefields((a,ap)::ar,(b,bp)::br) =
		ap=bp andalso same(a,b) andalso samefields(ar,br)
              | samefields(nil,nil) = true
              | samefields _ = false
	    fun samewith p = equ (p::pairs)
            fun all2 f (e::r,e'::r') = f(e,e') andalso all2 f (r,r')
              | all2 f (nil,nil) = true
              | all2 f _ = false
            val rec sameexp = 
	     fn (SELECT(i,v,w,_,e),SELECT(i',v',w',_,e')) =>
		   i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (RECORD(k,vl,w,e),RECORD(k',vl',w',e')) =>
		   (k = k') andalso samefields(vl,vl') 
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
	      | (RCC(p,vl,w,_,e),RCC(p',vl',w',_,e')) =>
		(* We don't need to compare protocol info:  The protocols are
		 * the same iff the functions and arguments are the same. *)
		all2 same (vl,vl') andalso samewith(w,w')(e,e')
	      | _ => false
        in  sameexp
        end
  in  equ nil (ce1,ce2)
  end

datatype info = FNinfo of {args: lvar list,
	       	           body : cexp option ref,
			   specialuse: int ref option ref,
			   liveargs : bool list option ref
			   }
	      | RECinfo of (value * accesspath) list
	      | SELinfo of int * value * cty
	      | OFFinfo of int * value
              | WRPinfo of P.pure * value
              | IFIDIOMinfo of {body : (lvar * cexp * cexp) option ref}
	      | MISCinfo of cty

fun contract {function=(fkind,fvar,fargs,ctyl,cexp), 
              table, click, last, size=cpssize} =
(* NOTE: the "last" argument is currently ignored. *)
let

val deadup = !Control.CG.deadup
val CGbetacontract = !Control.CG.betacontract
val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say(s) else ()
fun debugflush() = if debug then Control.Print.flush() else ()

val rep_flag = MachSpec.representations
val type_flag = (!CG.checkcps1) andalso (!CG.checkcps2) andalso rep_flag


(* It would be nice to get rid of this type stuff one day. *)
local

exception NCONTRACT 

fun valueName(VAR v) = LV.lvarName v
  | valueName(INT i) = "Int"^Int.toString(i)
  | valueName(REAL r) = "Real"^r
  | valueName(STRING s) = "<"^s^">"
  | valueName _ = "<others>"

fun argLty [] = LT.ltc_int
  | argLty [t] = 
      LT.ltw_tuple(t, 
            (fn xs as (_::_) => if (length(xs) < MachSpec.maxRepRegs)
                        then LT.ltc_tuple [t] else t
              | _ => t),
            fn t => 
               LT.ltw_str(t, 
                  (fn xs as (_::_) => if (length(xs) < MachSpec.maxRepRegs)
                              then LT.ltc_tuple [t] else t
                    | _ => t),
                  fn t => t))
  | argLty r = LT.ltc_str r (* this is INCORRECT !!!!!!! *)

val addty = if type_flag then IntHashTable.insert table else (fn _ => ())

in

(* Only used when dropping args in reduce(FIX) case. *)
fun getty v = 
  if type_flag then 
             (IntHashTable.lookup table v) handle _ =>
                   (Control.Print.say ("NCONTRACT: Can't find the variable "^
                            (Int.toString v)^" in the table ***** \n");
                    raise NCONTRACT)
  else LT.ltc_void
fun grabty u =
  let fun g(VAR v) = getty v
        | g(INT _) = LT.ltc_int
        | g(REAL _) = LT.ltc_real
        | g(STRING _) = LT.ltc_void
        | g(LABEL v) = getty v
        | g _ = LT.ltc_void
  in  if type_flag then g u
      else LT.ltc_void
  end
fun newty(f,t) = if type_flag then
		     (ignore (IntHashTable.remove table f) handle _ => ();
		      addty(f,t))
		 else ()
fun mkv(t) = let val v = LV.mkLvar()
                 val _ = addty(v,t)
             in  v
             end

fun ltc_fun (x, y) = 
  if (LT.ltp_tyc x) andalso (LT.ltp_tyc y) then LT.ltc_parrow(x, y)
  else LT.ltc_pfct(x, y)

fun mkfnLty(_,_,nil) = bug "mkfnLty in nflatten"
  | mkfnLty(k,CNTt::_,x::r) = 
      LT.ltw_iscont(x, fn [t2] => (k,ltc_fun(argLty r,t2))
                        | _ => bug "unexpected mkfnLty", 
             fn [t2] => (k,ltc_fun(argLty r, LT.ltc_tyc t2))
              | _ => bug "unexpected mkfnLty", 
             fn x => (k, ltc_fun(argLty r,x)))
  | mkfnLty(k,_,r) = (k, LT.ltc_cont([argLty r]))

(* Only used in newname *)
fun sameLty(x,u) = 
  let val s = (LV.lvarName(x))^(" *and* "^valueName(u))
  in  if type_flag then checklty s (getty x,grabty u)
      else ()
  end  

end (* local *)




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

fun enterREC(w,vl) = enter(w,{info=RECinfo vl, called=ref 0,used=ref 0})
fun enterMISC (w,ct) = enter(w,{info=MISCinfo ct, called=ref 0, used=ref 0})
val miscBOG = MISCinfo BOGt
fun enterMISC0 w = enter(w,{info=miscBOG, called=ref 0, used=ref 0})
fun enterWRP(w,p,u) = 
      enter(w,{info=WRPinfo(p,u), called=ref 0, used=ref 0})

fun enterFN (_,f,vl,cl,cexp) =
      (enter(f,{called=ref 0,used=ref 0,
		info=FNinfo{args=vl, 
			    body=ref(if CGbetacontract then SOME cexp
				     else NONE),
			    specialuse=ref NONE,
			    liveargs=ref NONE}});
       app2(enterMISC,vl,cl))

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
 fn RECORD(_,vl,w,e) => (enterREC(w,vl); app (use o #1) vl; g1 e)
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
  | BRANCH(i,vl,c,e1 as APP(VAR f1, [INT 1]),
		  e2 as APP(VAR f2, [INT 0])) =>
       (case get f1
	 of {info=FNinfo{body=ref(SOME(BRANCH(P.cmp{oper=P.neq,...},[INT 0, VAR w2],_,_,_))),
			 args=[w1],specialuse,...},...} => 
              (* Handle IF IDIOM *)
    	      if f1=f2 andalso w1=w2 
	      then let val {used,...}=get w1
		   in  specialuse := SOME used
		   end
	      else ()
	  | _ => ();
	app use vl; enterMISC(c,BOGt); g1 e1; g1 e2)
  | BRANCH(i,vl,c,e1,e2) => (app use vl; enterMISC0 c; g1 e1; g1 e2)
  | SETTER(i,vl,e) => (app use vl; g1 e)
  | LOOKER(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | ARITH(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | PURE(p as P.iwrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(p as P.iunwrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(p as P.i32wrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(p as P.i32unwrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(p as P.fwrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(p as P.funwrap,[u],w,_,e) => (use u; enterWRP(w,p,u); g1 e)
  | PURE(i,vl,w,_,e) => (app use vl; enterMISC0 w; g1 e)
  | RCC(p,vl,w,t,e) => (app use vl; enterMISC0 w; g1 e)
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
     sameLty vw; sameName vw; IntHashTable.insert m2 vw
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
  | drop_body(RCC(_,vl,_,_,e)) = (app use_less vl; drop_body e)
end (* local *)


fun setter (P.update, [_, _, INT _]) = P.unboxedupdate
  | setter (P.update, [_, _, REAL _]) = P.boxedupdate
  | setter (P.update, [_, _, STRING _]) = P.boxedupdate
  | setter (P.update, [_, _, VAR v]) = 
     (case #info(get v)
       of (FNinfo _) => P.boxedupdate
	| (RECinfo _) => P.boxedupdate
	| (OFFinfo _) => P.boxedupdate
	| _ => P.update
	(* end case *))
  | setter (P.assign, [_, INT _]) = P.unboxedassign
  | setter (i, _) = i

fun sameLvar(lvar, VAR lv) = lv = lvar
  | sameLvar _ = false

fun cvtPreCondition(n, n2, x, v2) =
  n=n2 andalso usedOnce(x) andalso sameLvar(x, ren v2) 

val rec reduce = fn cexp => g NONE cexp
and g = fn hdlr =>
let val rec g' =
  fn RECORD (k,vl,w,e) =>
      let val {used,...} = get w
	  val vl' = map (map1 ren) vl
       in if !used=0 andalso !CG.deadvars
	  then (click "b"; app (use_less o #1) vl'; g' e)
          else (let fun objlen(VAR z) =
                          (case (#info (get z))
                            of SELinfo(_,_,PTRt(RPT k)) => k
                             | SELinfo(_,_,PTRt(FPT k)) => k
                             | MISCinfo(PTRt(RPT k)) => k
                             | MISCinfo(PTRt(FPT k)) => k
                             | RECinfo l => length l
                             | _ => ~1)
                      | objlen _ = ~1
                             
                    fun samevar(VAR x,VAR y) = (x=y)
                      | samevar _ = false
 
                    fun check1((VAR z)::r,k,a) = 
                          (case (get z) 
                            of {info=SELinfo(i,b,_),...} => 
                                   (if ((i=k) andalso (samevar(ren b,a)))
                                    then check1(r,k+1,a) else NONE)
                             | _ => NONE)
                      | check1(_::r,k,_) = NONE 
                      | check1([],k,a) = 
                          if ((objlen a)=k) then SOME a else NONE
 
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
                              then (click "B"; app use_less vl''; e')
                              else RECORD(k, vl', w, e')
                          end)
                      | SOME z => 
                         (newname(w,z); click "B"; (*** ? ***)
                          app use_less vl''; g' e)
                end)
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
				     of {info=RECinfo vl,...} =>
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
		    of nil => [INT 0]
		     | [u] => 
                         LT.ltw_iscont(grabty u, 
                              fn _ => [u, INT 0],
                              fn _ => [u, INT 0],
                              fn _ => [u])
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
		      (* The code below may be obsolete.  I think that
			 we used to distinguish between user functions
			 and continuations in the closure phase by
		         the number of arguments, and also we might
			 not have been able to handle functions with
			 no arguments.  Possibly we can now remove
			 these special cases. *)
			  val tt' = map getty vl'
			  val (vl'', cl'', tt'') =
			      case tt'
			       of nil =>
				   let val x = mkv(LT.ltc_int)
				   in  dropclicks(drop - 1);
				       enterMISC0 x;
				       ([x],[INTt],[LT.ltc_int])
				   end
			        | [x] =>
                                   if (isCont x)
				   then let val x = mkv(LT.ltc_int)
				         in  dropclicks(drop - 1);
				             enterMISC0 x;
				             (vl'@[x], cl'@[INTt], 
                                              tt'@[LT.ltc_int])
				        end
                                   else (dropclicks(drop);
				       (vl',cl',tt'))
 			        | _ => (dropclicks(drop);
				       (vl',cl',tt'))

                          val (fk',lt) = mkfnLty(fk,cl'',tt'')
		      in  newty(f,lt);
			  ((fk',f,vl'',cl'',b),used,called,info)
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
	  val l2 = sublist keep l1
	  val e' = g' e
	  val l3 = sublist keep2 l2
	  val l4 = map reduce_body l3
      in  case (sublist keep3 l4)
	    of nil => e'
	     | l5 => FIX(map #1 l5, e')
      end
   | SWITCH(v,c,el) => 
      (case ren v
        of v' as INT i => 
	     if !CG.switchopt 
             then let fun f(e::el,j) = (if i=j then () else drop_body e;
					f(el,j+1))
		        | f(nil,_) = ()
		  in  click "h";
		       f(el,0);
		       newname(c,INT 0); 
		       g' (List.nth(el,i))
		  end
	     else SWITCH(v', c, map g' el)
	 | v' => SWITCH(v',c, map g' el))
   | LOOKER(P.gethdlr,_,w,t,e) =>
      (if !CG.handlerfold
       then case hdlr
             of NONE => if used w 
                        then LOOKER(P.gethdlr,[],w,t,g (SOME(VAR w)) e)
		        else (click "i"; g' e)
              | SOME w' => (click "j"; newname(w,w'); g' e)
       else LOOKER(P.gethdlr,[],w,t,g (SOME(VAR w)) e))
   | SETTER(P.sethdlr,[v as VAR vv],e) =>
      let val v' as VAR vv' = ren v
	  val e' = g (SOME v') e
      in  if !CG.handlerfold
	  then case hdlr 
		 of SOME (v'' as VAR vv'') => 
		     if vv'=vv'' then (click "k"; use_less v''; e')
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
      if cvtPreCondition(n, n2, x, v2) andalso n=m then
	(click "T(1)"; ARITH(P.test(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.test(p,n), [ren v], x, t, g' e)
   | ARITH(P.test(p,n),[v],x,t,e as ARITH(P.test(n2,m),[v2],x2,t2,e2)) => 
      if cvtPreCondition(n, n2, x, v2) then
	(click "T(2)"; ARITH(P.test(p,m), [ren v], x2, t2, g' e2))
      else ARITH(P.test(p,n), [ren v], x, t, g' e)
   | ARITH(P.testu(p,n),[v],x,t,e as PURE(P.copy(n2,m),[v2],x2,t2,e2)) =>
      if cvtPreCondition(n, n2, x, v2) andalso n=m then
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
   | PURE(P.extend(p,n), [v], x, t, e as PURE(pure, [v2], x2, t2, e2)) => let
       fun skip() = PURE(P.extend(p,n), [ren v], x, t, g' e)
       fun checkClicked(tok, n2, m, pureOp) = 
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok;
	    PURE(pureOp(p,m), [ren v], x2, t2, g' e2))
	 else skip()
     in
       case pure
	of P.extend(n2,m) => checkClicked("X(1)", n2, m, P.extend)
         | P.copy(n2,m) => 
	    if n2 = m then checkClicked("X(2)", n2, m, P.extend) else skip()
	 | P.trunc(n2,m) => 
	    if m >= p then checkClicked("X(3)", n2, m, P.extend)
	    else checkClicked("X(4)", n2, m, P.trunc)
	 | _ => skip()
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
   | PURE(P.copy(p,n), [v], x, t, e as PURE(pure, [v2], x2, t2, e2)) => let
       val v' = [ren v]
       fun skip () = PURE(P.copy(p,n), v', x, t, g' e)
       fun checkClicked(tok, n2, m, pureOp) = 
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok; PURE(pureOp(p,m), v', x2, t2, g' e2))
	 else skip()
     in
       case pure
	of P.copy(n2,m) => checkClicked("C(1)", n2, m, P.copy)
         | P.extend(n2,m) => 
	    if n > p then checkClicked("C(2)", n2, m, P.copy)
	    else if n = p then checkClicked("C(2)", n2, m, P.extend)
	    else skip()
   	 | P.trunc(n2,m) => 
            if m >= p then checkClicked("C(3)", n2, m, P.copy)
	    else if m < p then checkClicked("C(4)", n2, m, P.trunc)
	    else skip()
	 | _ => skip()
     end
   | PURE(P.copy(p,n), [v], x, t, e as ARITH(a, [v2], x2, t2, e2)) => let
       val v' = [ren v]
       fun skip () = PURE(P.copy(p,n), v', x, t, g' e)
       fun checkClicked(tok, n2, m, class, arithOp) = 
	 if cvtPreCondition(n, n2, x, v2) then
	   (click tok; class(arithOp(p,m), v', x2, t2, g' e2))
	 else skip()
     in
       case a
	of P.test(n2,m) =>
	   if m >= p then checkClicked("C5", n2, m, PURE, P.copy)
	   else checkClicked("C6", n2, m, ARITH, P.test)
	 | P.testu(n2,m) => 
	   if m > p then checkClicked("C7", n2, m, PURE, P.copy)
	   else checkClicked("C8", n2, m, ARITH, P.testu)
	 | _ => skip()
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
   | RCC(p,vl,w,t,e) =>
     (* leave raw C calls alone *)
     RCC (p, map ren vl, w, t, g' e)
   | BRANCH(i,vl,c,e1,e2) =>
      let val vl' = map ren vl
	  fun h() = (if !CG.branchfold andalso equalUptoAlpha(e1,e2)
		     then (click "z";
			   app use_less vl';
			   newname(c,INT 0);
			   drop_body e2;
			   g' e1)
		     else if !CG.comparefold
		     then if branch(i,vl') 
			       then (newname(c,INT 0); 
				     app use_less vl';
				     drop_body e2; 
				     g' e1)
			       else (newname(c,INT 0); 
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
           of (APP(VAR f, [INT 1]), APP(VAR f', [INT 0])) =>
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

 and branch =
    fn (P.unboxed, vl) => not(branch(P.boxed, vl))
     | (P.boxed, [INT _]) => (click "n"; false)
     | (P.boxed, [STRING s]) => (click "o"; true)
     | (P.boxed, [VAR v]) => 
	   (case get v
	     of {info=RECinfo _, ...} => (click "p"; true)
	      | _ => raise ConstFold)
     | (P.cmp{oper=P.<, kind}, [VAR v, VAR w]) => 
	   if v=w then (click "v"; false) else raise ConstFold
     | (P.cmp{oper=P.<, kind=P.INT 31}, [INT i, INT j]) => (click "w"; i<j)
     | (P.cmp{oper=P.>,kind}, [w,v]) =>
	   branch(P.cmp{oper=P.<,kind=kind},[v,w])
     | (P.cmp{oper=P.<=,kind}, [w,v]) =>
	   branch(P.cmp{oper=P.>=,kind=kind},[v,w])
     | (P.cmp{oper=P.>=,kind}, vl) =>
	   not(branch(P.cmp{oper=P.<,kind=kind}, vl))
     | (P.cmp{oper=P.<,kind=P.UINT 31}, [INT i, INT j]) => 
	   (click "w"; if j<0 then i>=0 orelse i<j else i>=0 andalso i<j) 
     | (P.cmp{oper=P.eql, kind}, [VAR v, VAR w]) => 
	 (case kind
  	   of P.FLOAT _ => raise ConstFold (* incase of NaN's *)
	    | _ => if v=w then  (click "v"; true) else raise ConstFold
 	           (*esac*))
     | (P.cmp{oper=P.eql,...}, [INT i, INT j]) => (click "w"; i=j)
     | (P.cmp{oper=P.neq,kind}, vl) => 
	   not(branch(P.cmp{oper=P.eql,kind=kind}, vl))
     | (P.peql, [INT i, INT j]) => (click "w"; i=j)
     | (P.pneq, [v,w]) => not(branch(P.peql,[w,v]))
     | _ => raise ConstFold

  and arith =
    fn (P.arith{oper=P.*,...}, [INT 1, v]) => (click "F"; v)
     | (P.arith{oper=P.*,...}, [v, INT 1]) => (click "G"; v)
     | (P.arith{oper=P.*,...}, [INT 0, _]) => (click "H"; INT 0)
     | (P.arith{oper=P.*,...}, [_, INT 0]) => (click "I"; INT 0)
     | (P.arith{oper=P.*,kind=P.INT 31}, [INT i, INT j]) =>
		let val x = i*j in x+x+2; click "J"; INT x end
     | (P.arith{oper=P./,...}, [v, INT 1]) => (click "K"; v)
     | (P.arith{oper=P./,...}, [INT i, INT 0]) => raise ConstFold
     | (P.arith{oper=P./,kind=P.INT 31}, [INT i, INT j]) =>
		let val x = Int.quot(i, j) in x+x; click "L"; INT x end
     | (P.arith{oper=P.+,...}, [INT 0, v]) => (click "M"; v)
     | (P.arith{oper=P.+,...}, [v, INT 0]) => (click "N"; v)
     | (P.arith{oper=P.+,kind=P.INT 31}, [INT i, INT j]) =>
	       let val x = i+j in x+x+2; click "O"; INT x end
     | (P.arith{oper=P.-,...}, [v, INT 0]) => (click "P"; v)
     | (P.arith{oper=P.-,kind=P.INT 31}, [INT i, INT j]) =>
	       let val x = i-j in x+x+2; click "Q"; INT x end
     | (P.arith{oper=P.~,kind=P.INT 31,...}, [INT i]) =>
		  let val x = ~i in x+x+2; click "X"; INT x end
     | _ => raise ConstFold

  and pure =
    fn (P.pure_arith{oper=P.rshift,kind=P.INT 31}, [INT i, INT j]) =>
	   (click "R"; INT(wtoi (Word.~>>(itow i, itow j))))
     | (P.pure_arith{oper=P.rshift,kind=P.INT 31}, [INT 0, _]) =>
	   (click "S"; INT 0)
     | (P.pure_arith{oper=P.rshift,kind=P.INT 31}, [v, INT 0]) =>
	   (click "T"; v)
     | (P.length, [STRING s]) => (click "V"; INT(size s))
(*         | (P.ordof, [STRING s, INT i]) => (click "W"; INT(ordof(s,i))) *)
     | (P.pure_arith{oper=P.lshift,kind=P.INT 31}, [INT i, INT j]) =>
		       (let val x = wtoi (Word.<<(itow i, itow j))
			in x+x; click "Y"; INT x
			end handle Overflow => raise ConstFold)
     | (P.pure_arith{oper=P.lshift,kind=P.INT 31}, [INT 0, _]) =>
	   (click "Z"; INT 0)
     | (P.pure_arith{oper=P.lshift,kind=P.INT 31}, [v, INT 0]) =>
	   (click "1"; v)
     | (P.pure_arith{oper=P.orb,kind=P.INT 31}, [INT i, INT j]) =>
	   (click "2"; INT(wtoi (Word.orb(itow i, itow j))))
     | (P.pure_arith{oper=P.orb,kind=P.INT 31}, [INT 0, v]) => (click "3"; v)
     | (P.pure_arith{oper=P.orb,kind=P.INT 31}, [v, INT 0]) => (click "4"; v)
     | (P.pure_arith{oper=P.xorb,kind=P.INT 31}, [INT i, INT j]) =>
	   (click "5"; INT(wtoi (Word.xorb(itow i, itow j))))
     | (P.pure_arith{oper=P.xorb,kind=P.INT 31}, [INT 0, v]) =>
	   (click "6"; v)
     | (P.pure_arith{oper=P.xorb,kind=P.INT 31}, [v, INT 0]) => (click "7"; v)
     | (P.pure_arith{oper=P.notb,kind=P.INT 31}, [INT i]) =>
	   (click "8"; INT(wtoi (Word.notb (itow i))))
     | (P.pure_arith{oper=P.andb,kind=P.INT 31}, [INT i, INT j]) =>
	   (click "9"; INT(wtoi(Word.andb(itow i, itow j))))
     | (P.pure_arith{oper=P.andb,kind=P.INT 31}, [INT 0, _]) =>
	   (click "0"; INT 0)
     | (P.pure_arith{oper=P.andb,kind=P.INT 31}, [_, INT 0]) =>
	   (click "T"; INT 0)
     | (P.real{fromkind=P.INT 31,tokind=P.FLOAT 64}, [INT i]) =>
	   (REAL(Int.toString i ^ ".0"))  (* isn't this cool? *)
     | (P.funwrap,[VAR v]) => 
          (case get(v) of {info=WRPinfo(P.fwrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
     | (P.fwrap,[VAR v]) =>
          (case get(v) of {info=WRPinfo(P.funwrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
     | (P.iunwrap,[VAR v]) =>
          (case get(v) of {info=WRPinfo(P.iwrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
     | (P.iwrap,[VAR v]) =>
          (case get(v) of {info=WRPinfo(P.iunwrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
     | (P.i32unwrap,[VAR v]) =>
          (case get(v) of {info=WRPinfo(P.i32wrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
     | (P.i32wrap,[VAR v]) =>
          (case get(v) of {info=WRPinfo(P.i32unwrap,u),...} => (click "U"; u)
                        | _ => raise ConstFold)
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

end (* toplevel local *)
end (* functor Contract *)


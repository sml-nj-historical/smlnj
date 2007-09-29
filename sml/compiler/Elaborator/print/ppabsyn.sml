(* Copyright 1992 by AT&T Bell Laboratories *)
(* absyn/ppabsyn.sml *)

signature PPABSYN =
sig
  val ppPat  : StaticEnv.staticEnv -> PrettyPrintNew.stream 
               -> Absyn.pat * int -> unit
  val ppExp  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrintNew.stream -> Absyn.exp * int -> unit
  val ppRule : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrintNew.stream -> Absyn.rule * int -> unit
  val ppVB   : StaticEnv.staticEnv * Source.inputSource option 
               -> PrettyPrintNew.stream -> Absyn.vb * int -> unit
  val ppRVB  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrintNew.stream -> Absyn.rvb * int -> unit
  val ppDec  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrintNew.stream -> Absyn.dec * int -> unit

  val ppStrexp : StaticEnv.staticEnv * Source.inputSource option
                 -> PrettyPrintNew.stream -> Absyn.strexp * int -> unit

  val lineprint : bool ref

  val debugging : bool ref

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN = 
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol
      structure PP = PrettyPrintNew
      structure PU = PPUtilNew

      open Absyn Tuples Fixity VarCon Types PrettyPrintNew PPUtilNew PPType PPVal
in

(* debugging *)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("PPAbsyn: "^msg)

val internals = ElabControl.internals

val lineprint = ref false

fun C f x y = f y x

val nullFix = INfix(0,0)
val infFix = INfix(1000000,100000)
fun strongerL(INfix(_,m),INfix(n,_)) = m >= n
  | strongerL _ = false			(* should not matter *)
fun strongerR(INfix(_,m),INfix(n,_)) = n > m
  | strongerR _ = true			(* should not matter *)

fun prpos(ppstrm: PP.stream,
          source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val (file:string,line:int,pos:int) = Source.filepos source charpos
       in PU.ppi ppstrm line;
	  PU.pps ppstrm ".";
	  PU.ppi ppstrm pos
      end
    else PU.ppi ppstrm charpos


fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) = 
    S.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun checkexp (n,nil) = true
  | checkexp (n, (LABEL{name=sym,...},_)::fields) = 
	S.eq(sym, numlabel n) andalso checkexp(n+1,fields)

fun isTUPLEpat (RECORDpat{fields=[_],...}) = false
  | isTUPLEpat (RECORDpat{flex=false,fields,...}) = checkpat(1,fields)
  | isTUPLEpat _ = false
	
fun isTUPLEexp (RECORDexp [_]) = false
  | isTUPLEexp (RECORDexp fields) = checkexp(1,fields)
  | isTUPLEexp (MARKexp(a,_)) = isTUPLEexp a
  | isTUPLEexp _ = false

fun lookFIX (env,sym) =
    Lookup.lookFix (env,S.fixSymbol(S.name sym))

fun stripMark (MARKexp(a,_)) = stripMark a
  | stripMark x = x

fun ppPat env ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
	fun ppPat' (_,0) = pps "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = pps "_"
	  | ppPat' (INTpat(i,t),_) = pps(IntInf.toString i)
(*	     (begin_block ppstrm INCONSISTENT 2;
	      pps "("; pps(IntInf.toString i);
	      pps " :"; break ppstrm {nsp=1,offset=1};
	      ppType env ppstrm t; pps ")";
	      end_block ppstrm) *)
	  | ppPat' (WORDpat(w,t),_) = pps(IntInf.toString w)
(*	     (openHOVBox 2;
	      pps "("; pps(IntInf.toString w);
	      pps " :"; break ppstrm {nsp=1,offset=1};
	      ppType env ppstrm t; pps ")";
	      closeBox ()) *)
	  | ppPat' (REALpat r,_) = pps r
	  | ppPat' (STRINGpat s,_) = pp_mlstr ppstrm s
	  | ppPat' (CHARpat s,_) = (pps "#"; pp_mlstr ppstrm s)
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (openHVBox 0;
	       ppPat'(v,d); pps " as "; ppPat'(p,d-1);
	       closeBox ())
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[],flex,...},_) =
	      if flex then pps "{...}"
	      else pps "()"
	  | ppPat' (r as RECORDpat{fields,flex,...},d) =
	      if isTUPLEpat r
	      then ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(C PP.string ")"),
		      pr=(fn _ => fn (sym,pat) => ppPat'(pat,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C PP.string "{"),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(fn ppstrm => if flex then PP.string ppstrm ",...}"
				         else PP.string ppstrm "}"),
		      pr=(fn ppstrm => fn (sym,pat) =>
			  (ppSym ppstrm sym; PP.string ppstrm "=";
			   ppPat'(pat,d-1))),
		      style=INCONSISTENT}
		     fields
	  | ppPat' (VECTORpat(nil,_), d) = pps "#[]"
	  | ppPat' (VECTORpat(pats,_), d) = 
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in ppClosedSequence ppstrm
		    {front=(C PP.string "#["),
		     sep=(fn ppstrm => (PP.string ppstrm ",";
					break ppstrm {nsp=0,offset=0})),
		     back=(C PP.string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    pats
	      end
	  | ppPat' (pat as (ORpat _), d) = let
	      fun mkList (ORpat(hd, tl)) = hd :: mkList tl
		| mkList p = [p]
	      fun pr _ pat = ppPat'(pat, d-1)
	      in
		ppClosedSequence ppstrm {
		    front = (C PP.string "("),
		    sep = fn ppstrm => (break ppstrm {nsp=1,offset=0};
                                        PP.string ppstrm "| "),
		    back = (C PP.string ")"),
		    pr = pr,
		    style = INCONSISTENT
		  } (mkList pat)
	      end
	  | ppPat' (CONpat(e,_),_) = ppDcon ppstrm e
	  | ppPat' (p as APPpat _, d) =
	      ppDconPat (env,ppstrm) (p,nullFix,nullFix,d)
	  | ppPat' (CONSTRAINTpat (p,t),d) =
	     (openHOVBox 0;
	      ppPat'(p,d-1); pps " :";
	      break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t;
	      closeBox ())
	  | ppPat' _ = bug "ppPat'"
     in ppPat'
    end

and ppDconPat(env,ppstrm) = 
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
	fun lpcond(atom) = if atom then pps "(" else ()
	fun rpcond(atom) = if atom then pps ")" else ()
	fun ppDconPat'(_,_,_,0) = pps "<pat>"
	  | ppDconPat'(CONpat(DATACON{name,...},_),l:fixity,r:fixity,_) =
	      ppSym ppstrm name
	  | ppDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	     (openHOVBox 0;
	      pps "("; ppPat env ppstrm (p,d-1); pps " :";
	      break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t; pps ")";
	      closeBox ())
	  | ppDconPat'(LAYEREDpat(v,p),l,r,d) =
	     (openHOVBox 0;
	      pps "("; ppPat env ppstrm (v,d); break ppstrm {nsp=1,offset=2};
	      pps " as "; ppPat env ppstrm (p,d-1); pps ")";
	      closeBox ())
	  | ppDconPat'(APPpat(DATACON{name,...},_,p),l,r,d) =
	      let val dname = S.name name 
		      (* should really have original path, like for VARexp *)
		  val thisFix = lookFIX(env,name)
		  val effFix = case thisFix of NONfix => infFix | x => x
		  val atom = strongerR(effFix,r) orelse strongerL(l,effFix)
	       in openHOVBox 2;
		  lpcond(atom);
		  case (thisFix,p)
		    of (INfix _, RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 let val (left,right) =
				 if atom then (nullFix,nullFix)
				 else (l,r)
			  in ppDconPat' (pl,left,thisFix,d-1);
			     break ppstrm {nsp=1,offset=0};
			     pps dname;
			     break ppstrm {nsp=1,offset=0};
			     ppDconPat' (pr,thisFix,right,d-1)
			 end
		     | _ =>
		        (pps dname; break ppstrm {nsp=1,offset=0};
			 ppDconPat'(p,infFix,infFix,d-1));
		  rpcond(atom);
		  closeBox ()
	      end
	  | ppDconPat' (p,_,_,d) = ppPat env ppstrm (p,d)
     in ppDconPat'
    end

fun trim [x] = []
  | trim (a::b) = a::trim b
  | trim [] = []

fun ppExp (context as (env,source_opt)) ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
	fun lparen() = pps "("
	fun rparen() = pps ")"
	fun lpcond(atom) = if atom then pps "(" else ()
	fun rpcond(atom) = if atom then pps ")" else ()
	fun ppExp'(_,_,0) = pps "<exp>"
	  | ppExp'(VARexp(ref var,_),_,_) = ppVar ppstrm var
	  | ppExp'(CONexp(con,_),_,_) = ppDcon ppstrm con
	  | ppExp'(INTexp (i,t),_,_) = pps(IntInf.toString i)
	  | ppExp'(WORDexp(w,t),_,_) = pps(IntInf.toString w)
	  | ppExp'(REALexp r,_,_) = pps r
	  | ppExp'(STRINGexp s,_,_) = pp_mlstr ppstrm s
	  | ppExp'(CHARexp s,_,_) = (pps "#"; pp_mlstr ppstrm s)
	  | ppExp'(r as RECORDexp fields,_,d) =
	      if isTUPLEexp r
	      then ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(C PP.string ")"),
		      pr=(fn _ => fn (_,exp) => ppExp'(exp,false,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C PP.string "{"),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(C PP.string "}"),
		      pr=(fn ppstrm => fn (LABEL{name,...},exp) =>
			  (ppSym ppstrm name; pps "=";
			   ppExp'(exp,false,d))),
		      style=INCONSISTENT}
		     fields
	  | ppExp'(SELECTexp (LABEL{name,...},exp),atom,d) =
	      (openHVBox 0;
	        lpcond(atom);
	        pps "#"; ppSym ppstrm name;
	        ppExp'(exp,true,d-1); pps ">";
		rpcond(atom);
	       closeBox ())
	  | ppExp'(VECTORexp(nil,_),_,d) = pps "#[]"
	  | ppExp'(VECTORexp(exps,_),_,d) =
	      let fun pr _ exp = ppExp'(exp,false,d-1)
	      in  ppClosedSequence ppstrm
		    {front=(C PP.string "#["),
		     sep=(fn ppstrm => (PP.string ppstrm ",";
					break ppstrm {nsp=1,offset=0})),
		     back=(C PP.string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    exps
	      end
          (*| ppExp'(PACKexp (e, t, tcs),atom,d) = 
        	      if !internals then
        		 (openHOVBox 0;
        		  pps "<PACK: "; ppExp'(e,false,d); pps "; ";
        		  break ppstrm {nsp=1,offset=2};
        		  ppType env ppstrm t; pps ">";
        		  closeBox ())
        	      else ppExp'(e,atom,d)*)
	  | ppExp'(SEQexp exps,_,d) =
	      ppClosedSequence ppstrm
	        {front=(C PP.string "("),
		 sep=(fn ppstrm => (PP.string ppstrm ";";
				    break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,false,d-1)),
		 style=INCONSISTENT}
		exps
	  | ppExp'(e as APPexp _,atom,d) =
	      let val infix0 = INfix(0,0)
	       in lpcond(atom);
		  ppAppExp(e,nullFix,nullFix,d);
		  rpcond(atom)
	      end
	  | ppExp'(CONSTRAINTexp(e, t),atom,d) =
	     (openHOVBox 0;
	       lpcond(atom);
	       ppExp'(e,false,d); pps ":";
	       break ppstrm {nsp=1,offset=2};
	       ppType env ppstrm t;
	       rpcond(atom);
	      closeBox ())
	  | ppExp'(HANDLEexp(exp, (rules,_)),atom,d) =
	     (openHVBox 0;
	       lpcond(atom);
	       ppExp'(exp,atom,d-1); newline ppstrm; pps "handle ";
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ())
	  | ppExp'(RAISEexp(exp,_),atom,d) = 
	      (openHVBox 0;
	       lpcond(atom);
	       pps "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ())
	  | ppExp'(LETexp(dec, exp),_,d) =
	      (openHVBox 0;
		pps "let ";
		openHVBox 0;
		 ppDec context ppstrm (dec,d-1); 
		closeBox ();
		break ppstrm {nsp=1,offset=0};
		pps "in ";
		openHVBox 0;
		 ppExp'(exp,false,d-1);
		closeBox ();
		break ppstrm {nsp=1,offset=0};
		pps "end";
	       closeBox ())
	  | ppExp'(CASEexp(exp, rules, _),_,d) =
	      (openHVBox 0;
	       pps "(case "; ppExp'(exp,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), 
                  trim rules);
	       rparen();
	       closeBox ())
	  | ppExp' (IFexp { test, thenCase, elseCase },atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       pps "if ";
	       openHVBox 0;
	        ppExp' (test, false, d-1);
	       closeBox ();
	       break ppstrm {nsp=1,offset= 0};
	       pps "then ";
	       openHVBox 0;
	        ppExp' (thenCase, false, d-1);
	       closeBox ();
	       break ppstrm {nsp=1,offset= 0};
	       pps "else ";
	       openHVBox 0;
	        ppExp' (elseCase, false, d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (ANDALSOexp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       break ppstrm {nsp=1,offset= 0};
	       pps "andalso ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (ORELSEexp (e1, e2),atom,d) =
	      (openHVBox 0;
	       lpcond(atom);
	       openHVBox 0;
	       ppExp' (e1,true,d-1);
	       closeBox ();
	       break ppstrm {nsp=1,offset= 0};
	       pps "orelse ";
	       openHVBox 0;
	       ppExp' (e2,true,d-1);
	       closeBox ();
	       rpcond(atom);
	       closeBox ())
	  | ppExp' (WHILEexp { test, expr },atom,d) =
	      (openHVBox 0;
	       pps "while ";
	       openHVBox 0;
	        ppExp'(test,false,d-1);
	       closeBox ();
	       break ppstrm {nsp=1,offset= 0};
	       pps "do ";
	       openHVBox 0;
	         ppExp'(expr,false,d-1);
	       closeBox ();
	       closeBox ())
	  | ppExp'(FNexp(rules,_),_,d) =
	      (openHVBox 0;
	       ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule context ppstrm (r,d-1)),
			       trim rules);
	       rparen();
	       closeBox ())
	  | ppExp'(MARKexp (exp,(s,e)),atom,d) =
	      (case source_opt
		of SOME source =>
		     if !internals
		     then (pps "<MARK(";
			   prpos(ppstrm,source,s); pps ",";
			   prpos(ppstrm,source,e); pps "): ";
			   ppExp'(exp,false,d); pps ">")
		     else ppExp'(exp,atom,d)
	         | NONE => ppExp'(exp,atom,d))

	and ppAppExp (_,_,_,0) = PP.string ppstrm "<exp>"
	  | ppAppExp arg =
	    let val pps = PP.string ppstrm
		fun fixitypp(name,rand,leftFix,rightFix,d) =
		    let val dname = SymPath.toString(SymPath.SPATH name)
			val thisFix = case name
					of [id] => lookFIX(env,id)
					 | _ => NONfix
			fun prNon exp =
			    (openHOVBox 2;
			     pps dname; break ppstrm {nsp=1,offset=0};
			     ppExp'(exp,true,d-1);
			     closeBox ())
		     in case thisFix
			  of INfix _ =>
			     (case stripMark rand
				of RECORDexp[(_,pl),(_,pr)] =>
				    let val atom = strongerL(leftFix,thisFix)
					     orelse strongerR(thisFix,rightFix)
					val (left,right) =
					    if atom then (nullFix,nullFix)
					    else (leftFix,rightFix)
				     in (openHOVBox 2;
					  lpcond(atom);
					  ppAppExp (pl,left,thisFix,d-1);
					  break ppstrm {nsp=1,offset=0};
					  pps dname;
					  break ppstrm {nsp=1,offset=0};
					  ppAppExp (pr,thisFix,right,d-1);
					  rpcond(atom);
					 closeBox ())
				    end
				 | e' => prNon e')
			   | NONfix => prNon rand
		    end
		fun appPrint(_,_,_,0) = pps "#"
		  | appPrint(APPexp(rator,rand),l,r,d) =
		    (case stripMark rator
		       of CONexp(DATACON{name,...},_) =>
		           fixitypp([name],rand,l,r,d)
		        | VARexp(v,_) =>
			   let val path = 
			           case !v
				     of VALvar{path=SymPath.SPATH p,...} => p
				      | OVLDvar{name,...} => [name]
				      | ERRORvar => [S.varSymbol "<errorvar>"]
			    in fixitypp(path,rand,l,r,d)
			   end
		        | rator =>
			   (openHOVBox 2;
			     ppExp'(rator,true,d-1); break ppstrm {nsp=1,offset=2};
			     ppExp'(rand,true,d-1);
			    closeBox ()))
		  | appPrint(MARKexp(exp,(s,e)),l,r,d) =
		      (case source_opt
			of SOME source =>
			     if !internals
			     then (pps "<MARK(";
				   prpos(ppstrm,source,s); pps ",";
				   prpos(ppstrm,source,e); pps "): ";
				   ppExp'(exp,false,d); pps ">")
			     else appPrint(exp,l,r,d)
			 | NONE => appPrint(exp,l,r,d))
		  | appPrint (e,_,_,d) = ppExp'(e,true,d)
	     in appPrint arg
	    end
     in (fn (exp,depth) => ppExp'(exp,false,depth))
    end

and ppRule (context as (env,source_opt)) ppstrm (RULE(pat,exp),d) =
    if d>0
    then (openHVBox ppstrm (Rel 0);
	  ppPat env ppstrm (pat,d-1);
	  PP.string ppstrm " =>"; break ppstrm {nsp=1,offset=2};
	  ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<rule>"

and ppVB (context as (env,source_opt)) ppstrm (VB{pat,exp,...},d) =
    if d>0
    then (openHVBox ppstrm (Rel 0);
	  ppPat env ppstrm (pat,d-1); PP.string ppstrm " =";
	  break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<binding>"

and ppRVB context ppstrm (RVB{var, exp, ...},d) = 
    if d>0
    then (openHOVBox ppstrm (Rel 0);
	  ppVar ppstrm var; PP.string ppstrm " =";
	  break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<rec binding>"

and ppDec (context as (env,source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm

      fun ppDec'(_,0) = pps "<dec>"
        | ppDec'(VALdec vbs,d) =
	  (openHVBox 0;
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVB context ppstrm (vb,d-1)),vbs);
	   closeBox ())
        | ppDec'(VALRECdec rvbs,d) =
	  (openHVBox 0;
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRVB context ppstrm (rvb,d-1)),rvbs);
	   closeBox ())
        | ppDec'(TYPEdec tycs,d) = let
	      fun f ppstrm (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (case arity
		    of 0 => ()
		     | 1 => (pps "'a ")
		     | n => (ppTuple ppstrm PP.string (typeFormals n); 
                             pps " ");
		   ppSym ppstrm (InvPath.last path);
		   pps " = "; ppType env ppstrm body)
		| f _ _ = bug "ppDec'(TYPEdec)"
	  in
	      openHVBox 0;
	      ppvlist ppstrm ("type "," and ", f, tycs);
	      closeBox ()
	  end
        | ppDec'(DATATYPEdec{datatycs,withtycs},d) = let
	      fun ppDATA ppstrm (GENtyc { path, arity, kind, ... }) =
		  (case kind of
		       DATATYPE(_) =>
		       (case arity
			 of 0 => ()
			  | 1 => (pps "'a ")
			  | n => (ppTuple ppstrm PP.string (typeFormals n); 
				  pps " ");
			ppSym ppstrm (InvPath.last path); pps " = ..."(*;
		        ppSequence ppstrm
			{sep=(fn ppstrm => (PP.string ppstrm " |";
					    break ppstrm {nsp=1,offset=0})),
			 pr=(fn ppstrm => fn (DATACON{name,...}) =>  
					     ppSym ppstrm name),
			 style=INCONSISTENT}
			dcons*))
		     | _ => bug "ppDec'(DATATYPEdec) 1.1")
		| ppDATA _ _ = bug "ppDec'(DATATYPEdec) 1.2"
	      fun ppWITH ppstrm (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (case arity
		    of 0 => ()
		     | 1 => (pps "'a ")
		     | n => (ppTuple ppstrm PP.string (typeFormals n); 
                             pps " ");
		   ppSym ppstrm (InvPath.last path);
		   pps " = "; ppType env ppstrm body)
		| ppWITH _ _ = bug "ppDec'(DATATYPEdec) 2"
	  in
	      (* could call PPDec.ppDec here *)
	      openHVBox 0;
	      ppvlist ppstrm ("datatype ","and ", ppDATA, datatycs);
	      newline ppstrm;
	      ppvlist ppstrm ("withtype ","and ", ppWITH, withtycs);
	      closeBox ()
	  end
        | ppDec'(ABSTYPEdec _,d) = pps "abstype"

        | ppDec'(EXCEPTIONdec ebs,d) = let
	      fun f ppstrm (EBgen{exn=DATACON{name,...},etype,...}) =
		  (ppSym ppstrm name;
		   case etype
		    of NONE => ()
		     | SOME ty' => (pps " of "; ppType env ppstrm ty'))
		| f ppstrm (EBdef{exn=DATACON{name,...},
				  edef=DATACON{name=dname,...}}) =
		  (ppSym ppstrm name; pps "="; ppSym ppstrm dname)
	  in
	      openHVBox 0;
	      ppvlist ppstrm ("exception ","and ", f, ebs);
	      closeBox ()
	  end
        | ppDec'(STRdec sbs,d) = let
	      fun f ppstrm (STRB{name, str=M.STR { access, ... }, def}) =
		  (ppSym ppstrm name;
		   ppAccess ppstrm access;
		   pps " = ";
		   break ppstrm {nsp=1,offset=2};
		   ppStrexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec:STRdec:STRB"
	  in
	      openHVBox 0;
	      ppvlist ppstrm ("structure ","and ", f, sbs);
	      closeBox ()
	  end
        | ppDec'(ABSdec sbs,d) = let
	      fun f ppstrm (STRB{name, str=M.STR { access, ... }, def}) =
		  (ppSym ppstrm name;
		   ppAccess ppstrm access;
		   pps " = ";
 		   break ppstrm {nsp=1,offset=2};
		   ppStrexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':ABSdec"
	  in
	      openHVBox 0;
	      ppvlist ppstrm ("abstraction ","and ", f, sbs);
	      closeBox ()
	  end
        | ppDec'(FCTdec fbs,d) = let
	      fun f ppstrm (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                  (ppSym ppstrm fname;
		   ppAccess ppstrm access;
		   pps " = "; 
		   break ppstrm {nsp=1,offset= 2}; 
		   ppFctexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':FCTdec"
	  in
	      openHVBox 0;
	      ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ()
	  end
        | ppDec'(SIGdec sigvars,d) = let
	      fun f ppstrm (M.SIG { name, ... }) =
		  (pps "signature "; 
		   case name of
		       SOME s => ppSym ppstrm s
                     | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':SIGdec"
	  in
	      openHVBox 0;
	      ppSequence ppstrm {sep=newline, pr=f,
				 style=CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(FSIGdec sigvars,d) = let
	      fun f ppstrm (M.FSIG{kind, ...}) =
		  (pps "funsig "; 
                   case kind of SOME s => ppSym ppstrm s
                              | NONE => pps "ANONYMOUS")
		| f _ _ = bug "ppDec':FSIGdec"
	  in
	      openHVBox 0;
	      ppSequence ppstrm
			 {sep=newline, pr = f, style = CONSISTENT} sigvars;
	      closeBox ()
	  end
        | ppDec'(LOCALdec(inner,outer),d) =
	  (openHVBox 0;
	   pps "local"; nl_indent ppstrm 2;
	   ppDec'(inner,d-1); newline ppstrm;
	   pps "in ";
	   ppDec'(outer,d-1); newline ppstrm;
	   pps "end";
	   closeBox ())
        | ppDec'(SEQdec decs,d) =
	  (openHVBox 0;
	   ppSequence ppstrm
	     {sep=newline,
	      pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	      style=CONSISTENT}
	     decs;
	   closeBox ())
        | ppDec'(FIXdec {fixity,ops},d) =
	  (openHVBox 0;
	   case fixity
	     of NONfix => pps "nonfix "
	      | INfix (i,_) => 
		    (if i mod 2 = 0 then 
		       pps "infix "
		     else pps "infixr ";
		     if i div 2 > 0 then
		       (pps(Int.toString(i div 2));
			pps " ")
		     else ());
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=ppSym,style=INCONSISTENT}
	     ops;
	   closeBox ())

        | ppDec'(OVLDdec ovldvar,d) =
	  (pps "overload "; ppVar ppstrm ovldvar)

        | ppDec'(OPENdec strbs,d) =
	  (openHVBox 0;
	   pps "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=(fn ppstrm => fn (sp,_) => 
                        pps (SymPath.toString sp)),
	      style=INCONSISTENT}
            strbs;
	   closeBox ())

        | ppDec'(MARKdec(dec,(s,e)),d) = 
	  (case source_opt
	    of SOME source =>
	       (pps "MARKdec(";
		ppDec'(dec,d); pps ",";
		prpos(ppstrm,source,s); pps ",";
		prpos(ppstrm,source,e); pps ")")

	     | NONE => ppDec'(dec,d))

     in ppDec'
    end

and ppStrexp (context as (_,source_opt)) ppstrm =
  let val pps = PP.string ppstrm
      fun ppStrexp'(_,0) = pps "<strexp>"

	| ppStrexp'(VARstr (M.STR { access, ... }), d) = ppAccess ppstrm access

	| ppStrexp'(APPstr{oper=M.FCT { access = fa, ... },
			   arg=M.STR { access = sa, ... }, ...}, d) =
	      (ppAccess ppstrm fa; pps"("; ppAccess ppstrm sa; pps")")
        | ppStrexp'(STRstr bindings, d) =
              (openHVBox ppstrm (Rel 0);
               pps "struct"; nl_indent ppstrm 2;
(*               pps "..."; *)
               (* ppBinding not yet undefined *)
                 ppSequence ppstrm
                   {sep=newline,
                    pr=(fn ppstrm => fn (B.VALbind v) => ppVar ppstrm v | b => pps "#"),
                          (*ppBinding context ppstrm (b,d-1)),*)
                    style=CONSISTENT}
                 bindings;
               pps "end";
               closeBox ppstrm)
	| ppStrexp'(LETstr(dec,body),d) =
	      (openHVBox ppstrm (Rel 0);
	       pps "let "; ppDec context ppstrm (dec,d-1); 
               newline ppstrm;
	       pps " in "; ppStrexp'(body,d-1); newline ppstrm;
	       pps "end";
	       closeBox ppstrm)
        | ppStrexp'(MARKstr(body,(s,e)),d) =
	      (case source_opt
		of SOME source =>
	           (pps "MARKstr(";
		    ppStrexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
	         | NONE => ppStrexp'(body,d))

        | ppStrexp' _ = bug "unexpected structure expression in ppStrexp'"

   in ppStrexp'
  end

and ppFctexp (context as (_,source_opt)) ppstrm = 
  let val pps = PP.string ppstrm

      fun ppFctexp'(_, 0) = pps "<fctexp>"
        | ppFctexp'(VARfct (M.FCT { access, ... }), d) = ppAccess ppstrm access

        | ppFctexp'(FCTfct{param=M.STR { access, ... }, def, ...}, d) =
            (pps " FCT("; 
	     ppAccess ppstrm access;
	     pps ") => "; newline ppstrm;
 	     ppStrexp context ppstrm (def,d-1))

        | ppFctexp'(LETfct(dec,body),d) =
	    (openHVBox ppstrm (Rel 0);
	     pps "let "; ppDec context ppstrm (dec,d-1); 
             newline ppstrm;
	     pps " in "; ppFctexp'(body,d-1); newline ppstrm;
	     pps "end";
	     closeBox ppstrm)

	| ppFctexp'(MARKfct(body,(s,e)),d) =
	    (case source_opt
	      of SOME source =>
	           (pps "MARKfct(";
		    ppFctexp'(body,d); pps ",";
		    prpos(ppstrm,source,s); pps ",";
		    prpos(ppstrm,source,e); pps ")")
               | NONE => ppFctexp'(body,d))

        | ppFctexp' _ = bug "unexpected functor expression in ppFctexp'"

   in ppFctexp'
  end

end (* top-level local *)
end (* structure PPAbsyn *)

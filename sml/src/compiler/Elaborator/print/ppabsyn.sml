(* Copyright 1992 by AT&T Bell Laboratories *)
(* absyn/ppabsyn.sml *)

signature PPABSYN =
sig
  val ppPat  : StaticEnv.staticEnv -> PrettyPrint.stream 
               -> Absyn.pat * int -> unit
  val ppExp  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.exp * int -> unit
  val ppRule : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.rule * int -> unit
  val ppVB   : StaticEnv.staticEnv * Source.inputSource option 
               -> PrettyPrint.stream -> Absyn.vb * int -> unit
  val ppRVB  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.rvb * int -> unit
  val ppDec  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.stream -> Absyn.dec * int -> unit

  val ppStrexp : StaticEnv.staticEnv * Source.inputSource option
                 -> PrettyPrint.stream -> Absyn.strexp * int -> unit

  val lineprint : bool ref

  val debugging : bool ref

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN = 
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol
      structure PP = PrettyPrint

      open Absyn Tuples Fixity VarCon Types PrettyPrint PPUtil PPType PPVal
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

fun prpos(ppstrm: PrettyPrint.stream,
          source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val (file:string,line:int,pos:int) = Source.filepos source charpos
       in PP.string ppstrm (Int.toString line);
	  PP.string ppstrm ".";
	  PP.string ppstrm (Int.toString pos)
      end
    else PP.string ppstrm (Int.toString charpos)


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
    let val ppsay = PP.string ppstrm
	fun ppPat' (_,0) = ppsay "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = ppsay "_"
	  | ppPat' (INTpat(i,t),_) = ppsay(IntInf.toString i)
(*	     (begin_block ppstrm INCONSISTENT 2;
	      ppsay "("; ppsay(IntInf.toString i);
	      ppsay " :"; break ppstrm {nsp=1,offset=1};
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm) *)
	  | ppPat' (WORDpat(w,t),_) = ppsay(IntInf.toString w)
(*	     (openStyleBox INCONSISTENT ppstrm (PP.Rel 2);
	      ppsay "("; ppsay(IntInf.toString w);
	      ppsay " :"; break ppstrm {nsp=1,offset=1};
	      ppType env ppstrm t; ppsay ")";
	      closeBox ppstrm) *)
	  | ppPat' (REALpat r,_) = ppsay r
	  | ppPat' (STRINGpat s,_) = pp_mlstr ppstrm s
	  | ppPat' (CHARpat s,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppPat'(v,d); ppsay " as "; ppPat'(p,d-1);
	       closeBox ppstrm)
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[],flex,...},_) =
	      if flex then ppsay "{...}"
	      else ppsay "()"
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
	  | ppPat' (VECTORpat(nil,_), d) = ppsay "#[]"
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
	     (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
	      ppPat'(p,d-1); ppsay " :";
	      break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t;
	      closeBox ppstrm)
	  | ppPat' _ = bug "ppPat'"
     in ppPat'
    end

and ppDconPat(env,ppstrm) = 
    let val ppsay = PP.string ppstrm
	fun lpcond(atom) = if atom then ppsay "(" else ()
	fun rpcond(atom) = if atom then ppsay ")" else ()
	fun ppDconPat'(_,_,_,0) = ppsay "<pat>"
	  | ppDconPat'(CONpat(DATACON{name,...},_),l:fixity,r:fixity,_) =
	      ppSym ppstrm name
	  | ppDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	     (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
	      ppsay "("; ppPat env ppstrm (p,d-1); ppsay " :";
	      break ppstrm {nsp=1,offset=2};
	      ppType env ppstrm t; ppsay ")";
	      closeBox ppstrm)
	  | ppDconPat'(LAYEREDpat(v,p),l,r,d) =
	     (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
	      ppsay "("; ppPat env ppstrm (v,d); break ppstrm {nsp=1,offset=2};
	      ppsay " as "; ppPat env ppstrm (p,d-1); ppsay ")";
	      closeBox ppstrm)
	  | ppDconPat'(APPpat(DATACON{name,...},_,p),l,r,d) =
	      let val dname = S.name name 
		      (* should really have original path, like for VARexp *)
		  val thisFix = lookFIX(env,name)
		  val effFix = case thisFix of NONfix => infFix | x => x
		  val atom = strongerR(effFix,r) orelse strongerL(l,effFix)
	       in openStyleBox INCONSISTENT ppstrm (PP.Rel 2);
		  lpcond(atom);
		  case (thisFix,p)
		    of (INfix _, RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 let val (left,right) =
				 if atom then (nullFix,nullFix)
				 else (l,r)
			  in ppDconPat' (pl,left,thisFix,d-1);
			     break ppstrm {nsp=1,offset=0};
			     ppsay dname;
			     break ppstrm {nsp=1,offset=0};
			     ppDconPat' (pr,thisFix,right,d-1)
			 end
		     | _ =>
		        (ppsay dname; break ppstrm {nsp=1,offset=0};
			 ppDconPat'(p,infFix,infFix,d-1));
		  rpcond(atom);
		  closeBox ppstrm
	      end
	  | ppDconPat' (p,_,_,d) = ppPat env ppstrm (p,d)
     in ppDconPat'
    end

fun trim [x] = []
  | trim (a::b) = a::trim b
  | trim [] = []

fun ppExp (context as (env,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
	fun lparen() = ppsay "("
	fun rparen() = ppsay ")"
	fun lpcond(atom) = if atom then ppsay "(" else ()
	fun rpcond(atom) = if atom then ppsay ")" else ()
	fun ppExp'(_,_,0) = ppsay "<exp>"
	  | ppExp'(VARexp(ref var,_),_,_) = ppVar ppstrm var
	  | ppExp'(CONexp(con,_),_,_) = ppDcon ppstrm con
	  | ppExp'(INTexp (i,t),_,_) = ppsay(IntInf.toString i)
	  | ppExp'(WORDexp(w,t),_,_) = ppsay(IntInf.toString w)
	  | ppExp'(REALexp r,_,_) = ppsay r
	  | ppExp'(STRINGexp s,_,_) = pp_mlstr ppstrm s
	  | ppExp'(STAMPexp _,_,_) = ppsay "<stamp>" (* FIXME *)
	  | ppExp'(CHARexp s,_,_) = (ppsay "#"; pp_mlstr ppstrm s)
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
			  (ppSym ppstrm name; ppsay "=";
			   ppExp'(exp,false,d))),
		      style=INCONSISTENT}
		     fields
	  | ppExp'(SELECTexp (LABEL{name,...},exp),atom,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	        lpcond(atom);
	        ppsay "#"; ppSym ppstrm name;
	        ppExp'(exp,true,d-1); ppsay ">";
		rpcond(atom);
	       closeBox ppstrm)
	  | ppExp'(VECTORexp(nil,_),_,d) = ppsay "#[]"
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
          | ppExp'(PACKexp (e, t, tcs),atom,d) = 
	      if !internals then
		 (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
		  ppsay "<PACK: "; ppExp'(e,false,d); ppsay "; ";
		  break ppstrm {nsp=1,offset=2};
		  ppType env ppstrm t; ppsay ">";
		  closeBox ppstrm)
	      else ppExp'(e,atom,d)
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
	     (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppExp'(e,false,d); ppsay ":";
	       break ppstrm {nsp=1,offset=2};
	       ppType env ppstrm t;
	       rpcond(atom);
	      closeBox ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER(FNexp(rules,_))),atom,d) =
	     (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppExp'(exp,atom,d-1); newline ppstrm; ppsay "handle ";
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER _),_,d) =
	      bug "ppExp'(HANDLEexp)"
	  | ppExp'(RAISEexp(exp,_),atom,d) = 
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppsay "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ppstrm)
	  | ppExp'(LETexp(dec, exp),_,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
		ppsay "let ";
		openStyleBox CONSISTENT ppstrm (PP.Rel 0);
		 ppDec context ppstrm (dec,d-1); 
		closeBox ppstrm;
		break ppstrm {nsp=1,offset=0};
		ppsay "in ";
		openStyleBox CONSISTENT ppstrm (PP.Rel 0);
		 ppExp'(exp,false,d-1);
		closeBox ppstrm;
		break ppstrm {nsp=1,offset=0};
		ppsay "end";
	       closeBox ppstrm)
	  | ppExp'(CASEexp(exp, rules, _),_,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppsay "(case "; ppExp'(exp,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), 
                  trim rules);
	       rparen();
	       closeBox ppstrm)
	  | ppExp' (IFexp { test, thenCase, elseCase },atom,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppsay "if ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	        ppExp' (test, false, d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "then ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	        ppExp' (thenCase, false, d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "else ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	        ppExp' (elseCase, false, d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	  | ppExp' (ANDALSOexp (e1, e2),atom,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppExp' (e1,true,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "andalso ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppExp' (e2,true,d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	  | ppExp' (ORELSEexp (e1, e2),atom,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       lpcond(atom);
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppExp' (e1,true,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "orelse ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppExp' (e2,true,d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	  | ppExp' (WHILEexp { test, expr },atom,d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppsay "while ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	        ppExp'(test,false,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "do ";
	       openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	         ppExp'(expr,false,d-1);
	       closeBox ppstrm;
	       closeBox ppstrm)
	  | ppExp'(FNexp(rules,_),_,d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule context ppstrm (r,d-1)),
			       trim rules);
	       rparen();
	       closeBox ppstrm)
	  | ppExp'(MARKexp (exp,(s,e)),atom,d) =
	      (case source_opt
		of SOME source =>
		     if !internals
		     then (ppsay "<MARK(";
			   prpos(ppstrm,source,s); ppsay ",";
			   prpos(ppstrm,source,e); ppsay "): ";
			   ppExp'(exp,false,d); ppsay ">")
		     else ppExp'(exp,atom,d)
	         | NONE => ppExp'(exp,atom,d))

	and ppAppExp (_,_,_,0) = PP.string ppstrm "<exp>"
	  | ppAppExp arg =
	    let val ppsay = PP.string ppstrm
		fun fixitypp(name,rand,leftFix,rightFix,d) =
		    let val dname = SymPath.toString(SymPath.SPATH name)
			val thisFix = case name
					of [id] => lookFIX(env,id)
					 | _ => NONfix
			fun prNon exp =
			    (openStyleBox INCONSISTENT ppstrm (PP.Rel 2);
			     ppsay dname; break ppstrm {nsp=1,offset=0};
			     ppExp'(exp,true,d-1);
			     closeBox ppstrm)
		     in case thisFix
			  of INfix _ =>
			     (case stripMark rand
				of RECORDexp[(_,pl),(_,pr)] =>
				    let val atom = strongerL(leftFix,thisFix)
					     orelse strongerR(thisFix,rightFix)
					val (left,right) =
					    if atom then (nullFix,nullFix)
					    else (leftFix,rightFix)
				     in (openStyleBox INCONSISTENT ppstrm (PP.Rel 2);
					  lpcond(atom);
					  ppAppExp (pl,left,thisFix,d-1);
					  break ppstrm {nsp=1,offset=0};
					  ppsay dname;
					  break ppstrm {nsp=1,offset=0};
					  ppAppExp (pr,thisFix,right,d-1);
					  rpcond(atom);
					 closeBox ppstrm)
				    end
				 | e' => prNon e')
			   | NONfix => prNon rand
		    end
		fun appPrint(_,_,_,0) = ppsay "#"
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
			   (openStyleBox INCONSISTENT ppstrm (PP.Rel 2);
			     ppExp'(rator,true,d-1); break ppstrm {nsp=1,offset=2};
			     ppExp'(rand,true,d-1);
			    closeBox ppstrm))
		  | appPrint(MARKexp(exp,(s,e)),l,r,d) =
		      (case source_opt
			of SOME source =>
			     if !internals
			     then (ppsay "<MARK(";
				   prpos(ppstrm,source,s); ppsay ",";
				   prpos(ppstrm,source,e); ppsay "): ";
				   ppExp'(exp,false,d); ppsay ">")
			     else appPrint(exp,l,r,d)
			 | NONE => appPrint(exp,l,r,d))
		  | appPrint (e,_,_,d) = ppExp'(e,true,d)
	     in appPrint arg
	    end
     in (fn (exp,depth) => ppExp'(exp,false,depth))
    end

and ppRule (context as (env,source_opt)) ppstrm (RULE(pat,exp),d) =
    if d>0
    then (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	  ppPat env ppstrm (pat,d-1);
	  PP.string ppstrm " =>"; break ppstrm {nsp=1,offset=2};
	  ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<rule>"

and ppVB (context as (env,source_opt)) ppstrm (VB{pat,exp,...},d) =
    if d>0
    then (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	  ppPat env ppstrm (pat,d-1); PP.string ppstrm " =";
	  break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<binding>"

and ppRVB context ppstrm (RVB{var, exp, ...},d) = 
    if d>0
    then (openStyleBox INCONSISTENT ppstrm (PP.Rel 0);
	  ppVar ppstrm var; PP.string ppstrm " =";
	  break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<rec binding>"

and ppDec (context as (env,source_opt)) ppstrm =
  let val ppsay = PP.string ppstrm

      fun ppDec'(_,0) = ppsay "<dec>"
        | ppDec'(VALdec vbs,d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVB context ppstrm (vb,d-1)),vbs);
	   closeBox ppstrm)
        | ppDec'(VALRECdec rvbs,d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRVB context ppstrm (rvb,d-1)),rvbs);
	   closeBox ppstrm)
        | ppDec'(TYPEdec tycs,d) = let
	      fun f ppstrm (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =
		  (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm PP.string (typeFormals n); 
                             ppsay " ");
		   ppSym ppstrm (InvPath.last path);
		   ppsay " = "; ppType env ppstrm body)
		| f _ _ = bug "ppDec'(TYPEdec)"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("type "," and ", f, tycs);
	      closeBox ppstrm
	  end
        | ppDec'(DATATYPEdec{datatycs,withtycs},d) = let
	      fun ppDATA ppstrm (GENtyc { path, arity, kind, ... }) =
		  (case kind of
		       DATATYPE(_) =>
		       (case arity
			 of 0 => ()
			  | 1 => (ppsay "'a ")
			  | n => (ppTuple ppstrm PP.string (typeFormals n); 
				  ppsay " ");
			ppSym ppstrm (InvPath.last path); ppsay " = ..."(*;
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
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm PP.string (typeFormals n); 
                             ppsay " ");
		   ppSym ppstrm (InvPath.last path);
		   ppsay " = "; ppType env ppstrm body)
		| ppWITH _ _ = bug "ppDec'(DATATYPEdec) 2"
	  in
	      (* could call PPDec.ppDec here *)
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("datatype ","and ", ppDATA, datatycs);
	      newline ppstrm;
	      ppvlist ppstrm ("withtype ","and ", ppWITH, withtycs);
	      closeBox ppstrm
	  end
        | ppDec'(ABSTYPEdec _,d) = ppsay "abstype"

        | ppDec'(EXCEPTIONdec ebs,d) = let
	      fun f ppstrm (EBgen{exn=DATACON{name,...},etype,...}) =
		  (ppSym ppstrm name;
		   case etype
		    of NONE => ()
		     | SOME ty' => (ppsay " of "; ppType env ppstrm ty'))
		| f ppstrm (EBdef{exn=DATACON{name,...},
				  edef=DATACON{name=dname,...}}) =
		  (ppSym ppstrm name; ppsay "="; ppSym ppstrm dname)
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("exception ","and ", f, ebs);
	      closeBox ppstrm
	  end
        | ppDec'(STRdec sbs,d) = let
	      fun f ppstrm (STRB{name, str=M.STR { access, ... }, def}) =
		  (ppSym ppstrm name;
		   ppAccess ppstrm access;
		   ppsay " = ";
		   break ppstrm {nsp=1,offset=2};
		   ppStrexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec:STRdec:STRB"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("structure ","and ", f, sbs);
	      closeBox ppstrm
	  end
        | ppDec'(ABSdec sbs,d) = let
	      fun f ppstrm (STRB{name, str=M.STR { access, ... }, def}) =
		  (ppSym ppstrm name;
		   ppAccess ppstrm access;
		   ppsay " = ";
		   break ppstrm {nsp=1,offset=2};
		   ppStrexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':ABSdec"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("abstraction ","and ", f, sbs);
	      closeBox ppstrm
	  end
        | ppDec'(FCTdec fbs,d) = let
	      fun f ppstrm (FCTB{name=fname, fct=M.FCT { access, ... }, def}) =
                  (ppSym ppstrm fname;
		   ppAccess ppstrm access;
		   ppsay " = "; 
		   break ppstrm {nsp=1,offset= 2}; 
		   ppFctexp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':FCTdec"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ppstrm
	  end
        | ppDec'(SIGdec sigvars,d) = let
	      fun f ppstrm (M.SIG { name, ... }) =
		  (ppsay "signature "; 
		   case name of
		       SOME s => ppSym ppstrm s
                     | NONE => ppsay "ANONYMOUS")
		| f _ _ = bug "ppDec':SIGdec"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppSequence ppstrm {sep=newline, pr=f,
				 style=CONSISTENT} sigvars;
	      closeBox ppstrm
	  end
        | ppDec'(FSIGdec sigvars,d) = let
	      fun f ppstrm (M.FSIG{kind, ...}) =
		  (ppsay "funsig "; 
                   case kind of SOME s => ppSym ppstrm s
                              | NONE => ppsay "ANONYMOUS")
		| f _ _ = bug "ppDec':FSIGdec"
	  in
	      openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	      ppSequence ppstrm
			 {sep=newline, pr = f, style = CONSISTENT} sigvars;
	      closeBox ppstrm
	  end
        | ppDec'(LOCALdec(inner,outer),d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   ppsay "local"; nl_indent ppstrm 2;
	   ppDec'(inner,d-1); newline ppstrm;
	   ppsay "in ";
	   ppDec'(outer,d-1); newline ppstrm;
	   ppsay "end";
	   closeBox ppstrm)
        | ppDec'(SEQdec decs,d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   ppSequence ppstrm
	     {sep=newline,
	      pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	      style=CONSISTENT}
	     decs;
	   closeBox ppstrm)
        | ppDec'(FIXdec {fixity,ops},d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   case fixity
	     of NONfix => ppsay "nonfix "
	      | INfix (i,_) => 
		    (if i mod 2 = 0 then 
		       ppsay "infix "
		     else ppsay "infixr ";
		     if i div 2 > 0 then
		       (ppsay(Int.toString(i div 2));
			ppsay " ")
		     else ());
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=ppSym,style=INCONSISTENT}
	     ops;
	   closeBox ppstrm)

        | ppDec'(OVLDdec ovldvar,d) =
	  (ppsay "overload "; ppVar ppstrm ovldvar)

        | ppDec'(OPENdec strbs,d) =
	  (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	   ppsay "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=(fn ppstrm => fn (sp,_) => 
                        ppsay (SymPath.toString sp)),
	      style=INCONSISTENT}
            strbs;
	   closeBox ppstrm)

        | ppDec'(MARKdec(dec,(s,e)),d) = 
	  (case source_opt
	    of SOME source =>
	       (ppsay "MARKdec(";
		ppDec'(dec,d); ppsay ",";
		prpos(ppstrm,source,s); ppsay ",";
		prpos(ppstrm,source,e); ppsay ")")

	     | NONE => ppDec'(dec,d))

     in ppDec'
    end

and ppStrexp (context as (_,source_opt)) ppstrm =
  let val ppsay = PP.string ppstrm
      fun ppStrexp'(_,0) = ppsay "<strexp>"

	| ppStrexp'(VARstr (M.STR { access, ... }), d) = ppAccess ppstrm access

	| ppStrexp'(APPstr{oper=M.FCT { access = fa, ... },
			   arg=M.STR { access = sa, ... }, ...}, d) =
	      (ppAccess ppstrm fa; ppsay"("; ppAccess ppstrm sa; ppsay")")
        | ppStrexp'(STRstr bindings, d) =
              (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
               ppsay "struct"; nl_indent ppstrm 2;
               ppsay "...";
               (* ppBinding not yet undefined *)
               (*
                 ppSequence ppstrm
                   {sep=newline,
                    pr=(fn ppstrm => fn b => ppBinding context ppstrm (b,d-1)),
                    style=CONSISTENT}
                 bindings;
                *)
               ppsay "end";
               closeBox ppstrm)
	| ppStrexp'(LETstr(dec,body),d) =
	      (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	       ppsay "let "; ppDec context ppstrm (dec,d-1); 
               newline ppstrm;
	       ppsay " in "; ppStrexp'(body,d-1); newline ppstrm;
	       ppsay "end";
	       closeBox ppstrm)
        | ppStrexp'(MARKstr(body,(s,e)),d) =
	      (case source_opt
		of SOME source =>
	           (ppsay "MARKstr(";
		    ppStrexp'(body,d); ppsay ",";
		    prpos(ppstrm,source,s); ppsay ",";
		    prpos(ppstrm,source,e); ppsay ")")
	         | NONE => ppStrexp'(body,d))

        | ppStrexp' _ = bug "unexpected structure expression in ppStrexp'"

   in ppStrexp'
  end

and ppFctexp (context as (_,source_opt)) ppstrm = 
  let val ppsay = PP.string ppstrm

      fun ppFctexp'(_, 0) = ppsay "<fctexp>"
        | ppFctexp'(VARfct (M.FCT { access, ... }), d) = ppAccess ppstrm access

        | ppFctexp'(FCTfct{param=M.STR { access, ... }, def, ...}, d) =
            (ppsay " FCT("; 
	     ppAccess ppstrm access;
	     ppsay ") => "; newline ppstrm;
 	     ppStrexp context ppstrm (def,d-1))

        | ppFctexp'(LETfct(dec,body),d) =
	    (openStyleBox CONSISTENT ppstrm (PP.Rel 0);
	     ppsay "let "; ppDec context ppstrm (dec,d-1); 
             newline ppstrm;
	     ppsay " in "; ppFctexp'(body,d-1); newline ppstrm;
	     ppsay "end";
	     closeBox ppstrm)

	| ppFctexp'(MARKfct(body,(s,e)),d) =
	    (case source_opt
	      of SOME source =>
	           (ppsay "MARKfct(";
		    ppFctexp'(body,d); ppsay ",";
		    prpos(ppstrm,source,s); ppsay ",";
		    prpos(ppstrm,source,e); ppsay ")")
               | NONE => ppFctexp'(body,d))

        | ppFctexp' _ = bug "unexpected functor expression in ppFctexp'"

   in ppFctexp'
  end

end (* top-level local *)
end (* structure PPAbsyn *)

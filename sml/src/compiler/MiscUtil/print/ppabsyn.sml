(* Copyright 1992 by AT&T Bell Laboratories *)
(* absyn/ppabsyn.sml *)

signature PPABSYN =
sig
  val ppPat  : StaticEnv.staticEnv -> PrettyPrint.ppstream 
               -> Absyn.pat * int -> unit
  val ppExp  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.ppstream -> Absyn.exp * int -> unit
  val ppRule : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.ppstream -> Absyn.rule * int -> unit
  val ppVB   : StaticEnv.staticEnv * Source.inputSource option 
               -> PrettyPrint.ppstream -> Absyn.vb * int -> unit
  val ppRVB  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.ppstream -> Absyn.rvb * int -> unit
  val ppDec  : StaticEnv.staticEnv * Source.inputSource option
               -> PrettyPrint.ppstream -> Absyn.dec * int -> unit

  val ppStrexp : StaticEnv.staticEnv * Source.inputSource option
                 -> PrettyPrint.ppstream -> Absyn.strexp * int -> unit

  val lineprint : bool ref

  val debugging : bool ref

end (* signature PPABSYN *)


structure PPAbsyn: PPABSYN = 
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol

      open Absyn Tuples Fixity VarCon Types PrettyPrint PPUtil PPType PPVal
in

(* debugging *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("PPAbsyn: "^msg)

val internals = Control.internals

val lineprint = ref false

fun C f x y = f y x

val nullFix = INfix(0,0)
val infFix = INfix(1000000,100000)
fun strongerL(INfix(_,m),INfix(n,_)) = m >= n
fun strongerR(INfix(_,m),INfix(n,_)) = n > m

fun prpos(ppstrm: PrettyPrint.ppstream,
          source: Source.inputSource, charpos: int) =
    if (!lineprint) then
      let val (file:string,line:int,pos:int) = Source.filepos source charpos
       in add_string ppstrm (Int.toString line);
	  add_string ppstrm ".";
	  add_string ppstrm (Int.toString pos)
      end
    else add_string ppstrm (Int.toString charpos)


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
    let val ppsay = add_string ppstrm
	fun ppPat' (_,0) = ppsay "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = ppsay "_"
	  | ppPat' (INTpat(i,t),_) = ppsay(IntInf.toString i)
(*	     (begin_block ppstrm INCONSISTENT 2;
	      ppsay "("; ppsay(IntInf.toString i);
	      ppsay " :"; add_break ppstrm (1,1);
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm) *)
	  | ppPat' (WORDpat(w,t),_) = ppsay(IntInf.toString w)
(*	     (begin_block ppstrm INCONSISTENT 2;
	      ppsay "("; ppsay(IntInf.toString w);
	      ppsay " :"; add_break ppstrm (1,1);
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm) *)
	  | ppPat' (REALpat r,_) = ppsay r
	  | ppPat' (STRINGpat s,_) = pp_mlstr ppstrm s
	  | ppPat' (CHARpat s,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppPat'(v,d); ppsay " as "; ppPat'(p,d-1);
	       end_block ppstrm)
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[],flex,...},_) =
	      if flex then ppsay "{...}"
	      else ppsay "()"
	  | ppPat' (r as RECORDpat{fields,flex,...},d) =
	      if isTUPLEpat r
	      then ppClosedSequence ppstrm
		     {front=(C add_string "("),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string ")"),
		      pr=(fn _ => fn (sym,pat) => ppPat'(pat,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C add_string "{"),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(fn ppstrm => if flex then add_string ppstrm ",...}"
				         else add_string ppstrm "}"),
		      pr=(fn ppstrm => fn (sym,pat) =>
			  (ppSym ppstrm sym; add_string ppstrm "=";
			   ppPat'(pat,d-1))),
		      style=INCONSISTENT}
		     fields
	  | ppPat' (VECTORpat(nil,_), d) = ppsay "#[]"
	  | ppPat' (VECTORpat(pats,_), d) = 
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in ppClosedSequence ppstrm
		    {front=(C add_string "#["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (0,0))),
		     back=(C add_string "]"),
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
		    front = (C add_string "("),
		    sep = fn ppstrm => (add_break ppstrm (1,0); add_string ppstrm "| "),
		    back = (C add_string ")"),
		    pr = pr,
		    style = INCONSISTENT
		  } (mkList pat)
	      end
	  | ppPat' (CONpat(e,_),_) = ppDcon ppstrm e
	  | ppPat' (p as APPpat _, d) =
	      ppDconPat (env,ppstrm) (p,nullFix,nullFix,d)
	  | ppPat' (CONSTRAINTpat (p,t),d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppPat'(p,d-1); ppsay " :";
	      add_break ppstrm (1,2);
	      ppType env ppstrm t;
	      end_block ppstrm)
	  | ppPat' _ = bug "ppPat'"
     in ppPat'
    end

and ppDconPat(env,ppstrm) = 
    let val ppsay = add_string ppstrm
	fun lpcond(atom) = if atom then ppsay "(" else ()
	fun rpcond(atom) = if atom then ppsay ")" else ()
	fun ppDconPat'(_,_,_,0) = ppsay "<pat>"
	  | ppDconPat'(CONpat(DATACON{name,...},_),l:fixity,r:fixity,_) =
	      ppSym ppstrm name
	  | ppDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppsay "("; ppPat env ppstrm (p,d-1); ppsay " :";
	      add_break ppstrm (1,2);
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm)
	  | ppDconPat'(LAYEREDpat(v,p),l,r,d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppsay "("; ppPat env ppstrm (v,d); add_break ppstrm (1,2);
	      ppsay " as "; ppPat env ppstrm (p,d-1); ppsay ")";
	      end_block ppstrm)
	  | ppDconPat'(APPpat(DATACON{name,...},_,p),l,r,d) =
	      let val dname = S.name name 
		      (* should really have original path, like for VARexp *)
		  val thisFix = lookFIX(env,name)
		  val effFix = case thisFix of NONfix => infFix | x => x
		  val atom = strongerR(effFix,r) orelse strongerL(l,effFix)
	       in begin_block ppstrm INCONSISTENT 2;
		  lpcond(atom);
		  case (thisFix,p)
		    of (INfix _, RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
			 let val (left,right) =
				 if atom then (nullFix,nullFix)
				 else (l,r)
			  in ppDconPat' (pl,left,thisFix,d-1);
			     add_break ppstrm (1,0);
			     ppsay dname;
			     add_break ppstrm (1,0);
			     ppDconPat' (pr,thisFix,right,d-1)
			 end
		     | _ =>
		        (ppsay dname; add_break ppstrm (1,0);
			 ppDconPat'(p,infFix,infFix,d-1));
		  rpcond(atom);
		  end_block ppstrm
	      end
	  | ppDconPat' (p,_,_,d) = ppPat env ppstrm (p,d)
     in ppDconPat'
    end

fun trim [x] = nil | trim (a::b) = a::trim b

fun ppExp (context as (env,source_opt)) ppstrm =
    let val ppsay = add_string ppstrm
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
	  | ppExp'(CHARexp s,_,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppExp'(r as RECORDexp fields,_,d) =
	      if isTUPLEexp r
	      then ppClosedSequence ppstrm
		     {front=(C add_string "("),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string ")"),
		      pr=(fn _ => fn (_,exp) => ppExp'(exp,false,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C add_string "{"),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string "}"),
		      pr=(fn ppstrm => fn (LABEL{name,...},exp) =>
			  (ppSym ppstrm name; ppsay "=";
			   ppExp'(exp,false,d))),
		      style=INCONSISTENT}
		     fields
	  | ppExp'(SELECTexp (LABEL{name,...},exp),atom,d) =
	      (begin_block ppstrm CONSISTENT 0;
	        lpcond(atom);
	        ppsay "#"; ppSym ppstrm name;
	        ppExp'(exp,true,d-1); ppsay ">";
		rpcond(atom);
	       end_block ppstrm)
	  | ppExp'(VECTORexp(nil,_),_,d) = ppsay "#[]"
	  | ppExp'(VECTORexp(exps,_),_,d) =
	      let fun pr _ exp = ppExp'(exp,false,d-1)
	      in  ppClosedSequence ppstrm
		    {front=(C add_string "#["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (1,0))),
		     back=(C add_string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    exps
	      end
          | ppExp'(PACKexp (e, t, tcs),atom,d) = 
	      if !internals then
		 (begin_block ppstrm INCONSISTENT 0;
		  ppsay "<PACK: "; ppExp'(e,false,d); ppsay "; ";
		  add_break ppstrm (1,2);
		  ppType env ppstrm t; ppsay ">";
		  end_block ppstrm)
	      else ppExp'(e,atom,d)
	  | ppExp'(SEQexp exps,_,d) =
	      ppClosedSequence ppstrm
	        {front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm ";";
				    add_break ppstrm (1,0))),
		 back=(C add_string ")"),
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
	     (begin_block ppstrm INCONSISTENT 0;
	       lpcond(atom);
	       ppExp'(e,false,d); ppsay ":";
	       add_break ppstrm (1,2);
	       ppType env ppstrm t;
	       rpcond(atom);
	      end_block ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER(FNexp(rules,_))),atom,d) =
	     (begin_block ppstrm CONSISTENT 0;
	       lpcond(atom);
	       ppExp'(exp,atom,d-1); add_newline ppstrm; ppsay "handle ";
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      end_block ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER _),_,d) =
	      bug "ppExp'(HANDLEexp)"
	  | ppExp'(RAISEexp(exp,_),atom,d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       lpcond(atom);
	       ppsay "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       end_block ppstrm)
	  | ppExp'(LETexp(dec, exp),_,d) =
	      (begin_block ppstrm CONSISTENT 0;
		ppsay "let ";
		begin_block ppstrm CONSISTENT 0;
		 ppDec context ppstrm (dec,d-1); 
		end_block ppstrm;
		add_break ppstrm (1,0);
		ppsay "in ";
		begin_block ppstrm CONSISTENT 0;
		 ppExp'(exp,false,d-1);
		end_block ppstrm;
		add_break ppstrm (1,0);
		ppsay "end";
	       end_block ppstrm)
	  | ppExp'(CASEexp(exp, rules, _),_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "(case "; ppExp'(exp,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), 
                  trim rules);
	       rparen();
	       end_block ppstrm)
	  | ppExp'(FNexp(rules,_),_,d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule context ppstrm (r,d-1)),
			       trim rules);
	       rparen();
	       end_block ppstrm)
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

	and ppAppExp (_,_,_,0) = add_string ppstrm "<exp>"
	  | ppAppExp arg =
	    let val ppsay = add_string ppstrm
		fun fixitypp(name,rand,leftFix,rightFix,d) =
		    let val dname = SymPath.toString(SymPath.SPATH name)
			val thisFix = case name
					of [id] => lookFIX(env,id)
					 | _ => NONfix
			fun prNon exp =
			    (begin_block ppstrm INCONSISTENT 2;
			     ppsay dname; add_break ppstrm (1,0);
			     ppExp'(exp,true,d-1);
			     end_block ppstrm)
		     in case thisFix
			  of INfix _ =>
			     (case stripMark rand
				of RECORDexp[(_,pl),(_,pr)] =>
				    let val atom = strongerL(leftFix,thisFix)
					     orelse strongerR(thisFix,rightFix)
					val (left,right) =
					    if atom then (nullFix,nullFix)
					    else (leftFix,rightFix)
				     in (begin_block ppstrm INCONSISTENT 2;
					  lpcond(atom);
					  ppAppExp (pl,left,thisFix,d-1);
					  add_break ppstrm (1,0);
					  ppsay dname;
					  add_break ppstrm (1,0);
					  ppAppExp (pr,thisFix,right,d-1);
					  rpcond(atom);
					 end_block ppstrm)
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
			   (begin_block ppstrm INCONSISTENT 2;
			     ppExp'(rator,true,d-1); add_break ppstrm (1,2);
			     ppExp'(rand,true,d-1);
			    end_block ppstrm))
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
    then (begin_block ppstrm CONSISTENT 0;
	  ppPat env ppstrm (pat,d-1);
	  add_string ppstrm " =>"; add_break ppstrm (1,2);
	  ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<rule>"

and ppVB (context as (env,source_opt)) ppstrm (VB{pat,exp,...},d) =
    if d>0
    then (begin_block ppstrm CONSISTENT 0;
	  ppPat env ppstrm (pat,d-1); add_string ppstrm " =";
	  add_break ppstrm (1,2); ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<binding>"

and ppRVB context ppstrm (RVB{var, exp, ...},d) = 
    if d>0
    then (begin_block ppstrm INCONSISTENT 0;
	  ppVar ppstrm var; add_string ppstrm " =";
	  add_break ppstrm (1,2); ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<rec binding>"

and ppDec (context as (env,source_opt)) ppstrm =
  let val ppsay = add_string ppstrm

      fun ppDec'(_,0) = ppsay "<dec>"
        | ppDec'(VALdec vbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVB context ppstrm (vb,d-1)),vbs);
	   end_block ppstrm)
        | ppDec'(VALRECdec rvbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRVB context ppstrm (rvb,d-1)),rvbs);
	   end_block ppstrm)
        | ppDec'(TYPEdec tycs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("type "," and ",
	    (fn ppstrm =>
	     (fn (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); 
                             ppsay " ");
		  ppSym ppstrm (InvPath.last path);
		  ppsay " = "; ppType env ppstrm body)
	       | _ => bug "ppDec'(TYPEdec)")),
	     tycs);
	   end_block ppstrm)
        | ppDec'(DATATYPEdec{datatycs,withtycs},d) =
	  (* could call PPDec.ppDec here *)
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("datatype ","and ",
	    (fn ppstrm =>
	     (fn GENtyc{path, arity, kind=DATATYPE(_),...} =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); 
                             ppsay " ");
		  ppSym ppstrm (InvPath.last path); ppsay " = ..."(*;
		  ppSequence ppstrm
		    {sep=(fn ppstrm => (add_string ppstrm " |";
					add_break ppstrm (1,0))),
		     pr=(fn ppstrm => fn (DATACON{name,...}) =>  
                             ppSym ppstrm name),
		     style=INCONSISTENT}
		    dcons*))
	       | _ => bug "ppDec'(DATATYPEdec) 1")),
	     datatycs);
	   add_newline ppstrm;
	   ppvlist ppstrm ("withtype ","and ",
	    (fn ppstrm =>
	     (fn (DEFtyc{path, tyfun=TYFUN{arity,body},...}) =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); 
                             ppsay " ");
		  ppSym ppstrm (InvPath.last path);
		  ppsay " = "; ppType env ppstrm body)
	       | _ => bug "ppDec'(DATATYPEdec) 2")),
	     withtycs);
	   end_block ppstrm)
        | ppDec'(ABSTYPEdec _,d) = ppsay "abstype"

        | ppDec'(EXCEPTIONdec ebs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("exception ","and ",
	    (fn ppstrm =>
	     (fn (EBgen{exn=DATACON{name,...},etype,...}) =>
		   (ppSym ppstrm name;
		    case etype
		      of NONE => ()
		       | SOME ty' =>
			  (ppsay " of "; ppType env ppstrm ty'))
	       | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
		   (ppSym ppstrm name; ppsay "="; ppSym ppstrm dname))),
	     ebs);
	   end_block ppstrm)

        | ppDec'(STRdec sbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("structure ","and ",
	    (fn ppstrm =>
	     (fn (STRB{name, str=M.STR{access,...}, def}) =>
		 (ppSym ppstrm name; ppAccess ppstrm access; ppsay " = ";
		  add_break ppstrm (1,2); ppStrexp context ppstrm (def,d-1)))),
	     sbs);
	   end_block ppstrm)

        | ppDec'(ABSdec sbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("abstraction ","and ",
	    (fn ppstrm =>
	     (fn (STRB{name, str=M.STR{access, ...}, def}) =>
		 (ppSym ppstrm name; ppAccess ppstrm access; ppsay " = ";
		  add_break ppstrm (1,2); ppStrexp context ppstrm (def,d-1)))),
	     sbs);
	   end_block ppstrm)

        | ppDec'(FCTdec fbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("functor ","and ",
	    (fn ppstrm =>
 	     (fn (FCTB{name=fname, fct=M.FCT{access=fctAcc,...}, def}) => 
                 (ppSym ppstrm fname; ppAccess ppstrm fctAcc; ppsay " = "; 
		    add_break ppstrm (1, 2); 
		    ppFctexp context ppstrm (def,d-1)))),
            fbs);
           end_block ppstrm)

        | ppDec'(SIGdec sigvars,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn M.SIG{name, ...} =>
		    (ppsay "signature "; 
                     case name of SOME s => ppSym ppstrm s
                                | NONE => ppsay "ANONYMOUS")),
	      style=CONSISTENT}
	     sigvars;
	   end_block ppstrm)

        | ppDec'(FSIGdec sigvars,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn M.FSIG{kind, ...} =>
		    (ppsay "funsig "; 
                     case kind of SOME s => ppSym ppstrm s
                                | NONE => ppsay "ANONYMOUS")),
	      style=CONSISTENT}
	     sigvars;
	   end_block ppstrm)

        | ppDec'(LOCALdec(inner,outer),d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppsay "local"; nl_indent ppstrm 2;
	   ppDec'(inner,d-1); add_newline ppstrm;
	   ppsay "in ";
	   ppDec'(outer,d-1); add_newline ppstrm;
	   ppsay "end";
	   end_block ppstrm)

        | ppDec'(SEQdec decs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	      style=CONSISTENT}
	     decs;
	   end_block ppstrm)

        | ppDec'(FIXdec {fixity,ops},d) =
	  (begin_block ppstrm CONSISTENT 0;
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
	     {sep=(fn ppstrm => add_break ppstrm (1,0)),
	      pr=ppSym,style=INCONSISTENT}
	     ops;
	   end_block ppstrm)

        | ppDec'(OVLDdec ovldvar,d) =
	  (ppsay "overload "; ppVar ppstrm ovldvar)

        | ppDec'(OPENdec strbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppsay "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => add_break ppstrm (1,0)),
	      pr=(fn ppstrm => fn (sp,_) => 
                        ppsay (SymPath.toString sp)),
	      style=INCONSISTENT}
            strbs;
	   end_block ppstrm)

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
  let val ppsay = add_string ppstrm
      fun ppStrexp'(_,0) = ppsay "<strexp>"

	| ppStrexp'(VARstr (M.STR{access, ...}), d) = 
              ppAccess ppstrm access

	| ppStrexp'(APPstr{oper=M.FCT{access=facc,...}, 
                             arg=M.STR{access=sacc, ...}, ...}, d) =
	      (ppAccess ppstrm facc; ppsay"(";
	       ppAccess ppstrm sacc; ppsay")")
        | ppStrexp'(STRstr bindings, d) =
              (begin_block ppstrm CONSISTENT 0;
               ppsay "struct"; nl_indent ppstrm 2;
               ppsay "...";
               (* ppBinding not yet undefined *)
               (*
                 ppSequence ppstrm
                   {sep=add_newline,
                    pr=(fn ppstrm => fn b => ppBinding context ppstrm (b,d-1)),
                    style=CONSISTENT}
                 bindings;
                *)
               ppsay "end";
               end_block ppstrm)
	| ppStrexp'(LETstr(dec,body),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec context ppstrm (dec,d-1); 
               add_newline ppstrm;
	       ppsay " in "; ppStrexp'(body,d-1); add_newline ppstrm;
	       ppsay "end";
	       end_block ppstrm)
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
  let val ppsay = add_string ppstrm

      fun ppFctexp'(_, 0) = ppsay "<fctexp>"
        | ppFctexp'(VARfct (M.FCT{access, ...}), d) = 
            ppAccess ppstrm access

        | ppFctexp'(FCTfct{param=M.STR{access=strAcc, ...},  
                                  def, ...}, d) =
            (ppsay " FCT("; 
	     ppAccess ppstrm strAcc; ppsay ") => "; add_newline ppstrm;
 	     ppStrexp context ppstrm (def,d-1))

        | ppFctexp'(LETfct(dec,body),d) =
	    (begin_block ppstrm CONSISTENT 0;
	     ppsay "let "; ppDec context ppstrm (dec,d-1); 
             add_newline ppstrm;
	     ppsay " in "; ppFctexp'(body,d-1); add_newline ppstrm;
	     ppsay "end";
	     end_block ppstrm)

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

(*
 * $Log: ppabsyn.sml,v $
 * Revision 1.7  1997/12/17 15:18:31  dbm
 *   Fix for bug 1326.  Added ERRORvar rule to a case expression.
 *
 * Revision 1.6  1997/11/11  05:27:38  dbm
 *   Fix expression printing, especially infix expressions.
 *
 * Revision 1.5  1997/07/15  16:19:26  dbm
 *   Adjust to changes in signature representation.
 *
 * Revision 1.4  1997/04/14  21:37:28  dbm
 *   Eliminated redundant final rule in function ppExp'.
 *
 * Revision 1.3  1997/04/10  14:39:40  dbm
 *   Added missing SELECT case to ppExp'.
 *
 * Revision 1.2  1997/01/28  23:20:46  jhr
 * Integer and word literals are now represented by IntInf.int (instead of
 * as strings).
 *
 * Revision 1.1.1.1  1997/01/14  01:38:43  george
 *   Version 109.24
 *
 *)

(* Copyright 2003 by University of Chicago *)
(* Jing Cao and Lukasz Ziarek *)

structure PPAst: PPAST = 
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol
      structure BT = BasicTypes
      structure PP = PrettyPrintNew
      structure PU = PPUtilNew

      open Ast Tuples Fixity VarCon Types PrettyPrintNew PPUtilNew PPType PPVal
in

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
       in PP.string ppstrm (Int.toString line);
	  PP.string ppstrm ".";
	  PP.string ppstrm (Int.toString pos)
      end
    else PP.string ppstrm (Int.toString charpos)

fun bug msg = ErrorMsg.impossible("PPAst: "^msg)

val arrowStamp = BT.arrowStamp

fun strength(ty) =
    case ty
      of VarTy(_) => 1
       | ConTy(tycon, args) => 
          (case tycon
            of [tyc] =>
	         if S.eq(S.tycSymbol("->"), tyc) then 0
		 else 2
	     | _ => 2)
       | RecordTy _ => 2
       | TupleTy _ => 1
       | _ => 2

fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) = S.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun checkexp (n, nil) = true
  | checkexp (n, (sym,exp)::fields) =
	S.eq(sym, numlabel n) andalso checkexp (n+1, fields)

fun isTUPLEpat (RecordPat{def=[_],...}) = false
  | isTUPLEpat (RecordPat{def=defs,flexibility=false}) = checkpat(1,defs)
  | isTUPLEpat _ = false

fun isTUPLEexp (RecordExp[_]) = false
  | isTUPLEexp (RecordExp fields) = checkexp(1,fields)
  | isTUPLEexp (MarkExp(a,_)) = isTUPLEexp a
  | isTUPLEexp _ = false

fun lookFIX (env,sym) =
    Lookup.lookFix (env,S.fixSymbol(S.name sym))

fun stripMark (MarkExp(a,_)) = stripMark a
  | stripMark x = x

fun trim [x] = []
  | trim (a::b) = a::trim b
  | trim [] = []


fun pp_path ppstrm symbols =
    let fun pr ppstrm (symbol) = (ppSym ppstrm symbol)
     in ppSequence ppstrm
         {sep=(fn ppstrm => (PP.string ppstrm ".")),
          pr=pr,
          style=INCONSISTENT}
         symbols
    end

fun ppPat (context as (env, source_opt)) ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppPat' (WildPat,_) = (pps "_")
	  | ppPat' (VarPat p, d) =  pp_symbol_list(p)
	  | ppPat' (IntPat i,_) = pps(IntInf.toString i)
	  | ppPat' (WordPat w,_) = pps(IntInf.toString w)
	  | ppPat' (StringPat s, _) = pp_mlstr ppstrm s
	  | ppPat' (CharPat s,_) = (pps "#"; pp_mlstr ppstrm s)
	  | ppPat' (LayeredPat {varPat,expPat},d) =
	      (openHVBox 0;
	       ppPat'(varPat,d); pps " as "; ppPat'(expPat,d-1);
	       closeBox ())
	  | ppPat' (RecordPat{def=[],flexibility},_) =
	      if flexibility then pps "{...}"
	      else pps "()"
	  | ppPat' (r as RecordPat{def,flexibility},d) =
              if isTUPLEpat r
              then ppClosedSequence ppstrm
                      {front=(C PP.string "("),
                       sep=(fn ppstrm => (PP.string ppstrm ",";
                                          break ppstrm {nsp=0,offset=0})),
                       back=(C PP.string ")"),
                       pr=(fn _ => fn (sym,pat) => ppPat' (pat, d-1)),
                       style=INCONSISTENT}
                      def
              else ppClosedSequence ppstrm
                      {front=(C PP.string "{"),
                       sep=(fn ppstrm => (PP.string ppstrm ",";
                                          break ppstrm {nsp=0,offset=0})),
                       back=(fn ppstrm => if flexibility then PP.string ppstrm ",...}"
                                          else PP.string ppstrm "}"),
                       pr=(fn ppstrm => fn (sym,pat) => (ppSym ppstrm sym;
                                                         PP.string ppstrm "=";
                           ppPat' (pat, d-1))),
                       style=INCONSISTENT}
                      def  
	  | ppPat' (ListPat nil, d) = pps "[]"
	  | ppPat' (ListPat l, d) =	
		let fun pr _ pat = ppPat'(pat, d-1)
		in ppClosedSequence ppstrm
		   {front=(C PP.string "["),
		    sep=(fn ppstrm => (PP.string ppstrm ",";
                                       break ppstrm {nsp=0,offset=0})),
		    back=(C PP.string "]"),
	 	    pr=pr,
		    style=INCONSISTENT}
		   l
		end
	  | ppPat' (TuplePat t, d) = 
	    	let fun pr _ pat = ppPat'(pat, d-1)
	    	in ppClosedSequence ppstrm
			     {front=(C PP.string "("),
			      sep=(fn ppstrm => (PP.string ppstrm ",";
						 break ppstrm {nsp=0,offset=0})),
			      back=(C PP.string ")"),
			      pr=pr,
			      style=INCONSISTENT}
			     t
	    	end
	  | ppPat' (FlatAppPat fap, d) =
		let fun pr _ {item,fixity,region} = ppPat'(item,d-1)		
		in ppSequence ppstrm
			{sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
			 pr=pr,
			 style=INCONSISTENT}
			fap
		end 
	  | ppPat' (AppPat {constr, argument}, d) = 
		(openHVBox 0;
		 ppPat'(constr,d); pps " as "; ppPat'(argument,d);
		 closeBox ())
	  | ppPat' (ConstraintPat {pattern, constraint}, d) = 
		(openHOVBox 0;
		 ppPat' (pattern, d-1); pps " :";
		 break ppstrm {nsp=1,offset=2};
		 ppTy context ppstrm (constraint, d);
		 closeBox ())
	  | ppPat' (VectorPat nil, d) = pps "#[]"
	  | ppPat' (VectorPat v, d) = 
		let fun pr _ pat = ppPat'(pat, d-1)
		in ppClosedSequence ppstrm
		   {front=(C PP.string "#["),
		    sep=(fn ppstrm => (PP.string ppstrm ",";break ppstrm {nsp=1,offset=0})),
		    back=(C PP.string "]"),
	 	    pr=pr,
		    style=INCONSISTENT}
		   v
		end
	  | ppPat' (MarkPat (pat, (s,e)), d) = 
	    (case source_opt
		of SOME source =>
		     if !internals
		     then (pps "<MARK(";
			   prpos(ppstrm,source,s); pps ",";
			   prpos(ppstrm,source,e); pps "): ";
			   ppPat'(pat,d); pps ">")
		     else ppPat'(pat,d)
	         | NONE => ppPat'(pat,d))

          | ppPat' (OrPat orpat, d) =
		let fun pr _ pat = ppPat'(pat, d-1)		
		in ppClosedSequence ppstrm
			{front=(C PP.string "("),
			 sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0}; PP.string ppstrm "| ")),
			 back=(C PP.string ")"),
			 pr=pr,
			 style=INCONSISTENT}
		end (orpat)
	
    in ppPat'
    end


and ppExp (context as (env, source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
      fun lparen() = pps "(" 
      fun rparen() = pps ")"
      fun lpcond(atom) = if atom then pps "(" else ()      
      fun rpcond(atom) = if atom then pps ")" else ()
      val pp_symbol_list = pp_path ppstrm
      fun ppExp' (_,_,0) = pps "<exp>"
	| ppExp' (VarExp p,_,_) = pp_symbol_list(p)
	| ppExp' (FnExp nil,_,d) = pps "<function>"
	| ppExp' (FnExp rules,_,d)=	
		let fun pr _ pat = ppRule context ppstrm(pat, d-1)
		in ppSequence ppstrm
		   {sep=(fn ppstrm => (PP.string ppstrm "|";break ppstrm {nsp=0,offset=0})),
	 	    pr=pr,
		    style=INCONSISTENT}
		   rules
		end

	| ppExp' (FlatAppExp fap,_,d) = 
		let fun pr _ {item,fixity,region} = ppExp'(item,true, d)		
		in ppSequence ppstrm
			{sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
			 pr=pr,
			 style=INCONSISTENT}
			fap
		end 
	| ppExp'(e as AppExp _,atom,d) =
	      let val infix0 = INfix(0,0)
	       in lpcond(atom);
		  ppAppExp(e,nullFix,nullFix,d);
		  rpcond(atom)
	      end
	| ppExp' (CaseExp {expr, rules},_,d) = 
	      (openHVBox 0;
	       pps "(case "; ppExp'(expr,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), 
                  trim rules);
	       rparen();
	       closeBox ())
	| ppExp' (LetExp {dec, expr},_,d) =
	      (openHVBox 0;
		pps "let ";
		openHVBox 0;
		 ppDec context ppstrm (dec,d-1); 
		closeBox ();
		break ppstrm {nsp=1,offset=0};
		pps "in ";
		openHVBox 0;
		 ppExp'(expr,false,d-1);
		closeBox ();
		break ppstrm {nsp=1,offset=0};
		pps "end";
	       closeBox ())
 	| ppExp'(SeqExp exps,_,d) =
	      ppClosedSequence ppstrm
	        {front=(C PP.string "("),
		 sep=(fn ppstrm => (PP.string ppstrm ";";
				    break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,false,d-1)),
		 style=INCONSISTENT}
		exps
	| ppExp' (IntExp i,_,_) = pps (IntInf.toString i)
	| ppExp' (WordExp w,_,_) = pps (IntInf.toString w)
	| ppExp' (RealExp r,_,_) = pps r
	| ppExp' (StringExp s,_,_) = pp_mlstr ppstrm s
	| ppExp' (CharExp s,_,_) = (pps "#"; pp_mlstr ppstrm s)
	| ppExp'(r as RecordExp fields,_,d) =
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
		      pr=(fn ppstrm => fn (name,exp) =>
			  (ppSym ppstrm name; pps "=";
			   ppExp'(exp,false,d))),
		      style=INCONSISTENT}
		     fields
	| ppExp' (ListExp p,_,d) = 
		 ppClosedSequence ppstrm
		     {front=(C PP.string "["),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(C PP.string "]"),
		      pr=(fn ppstrm => fn exp =>
			  (ppExp'(exp,false,d-1))),
		      style=INCONSISTENT}
		     p
	| ppExp' (TupleExp p,_,d)=
		ppClosedSequence ppstrm
		     {front=(C PP.string "("),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 break ppstrm {nsp=0,offset=0})),
		      back=(C PP.string ")"),
		      pr=(fn ppstrm => fn exp =>
			  (ppExp'(exp,false,d-1))),
		      style=INCONSISTENT}
		     p
	| ppExp'(SelectorExp name, atom,d) =
	      (openHVBox 0;
	        lpcond(atom);
	        pps "#"; ppSym ppstrm name;
	        pps ">";
		rpcond(atom);
	       closeBox ())
	| ppExp' (ConstraintExp {expr,constraint},atom,d) = 
	     (openHOVBox 0;
	       lpcond(atom);
	       ppExp'(expr,false,d); pps ":";
	       break ppstrm {nsp=1,offset=2};
	       ppTy context ppstrm (constraint,d);
	       rpcond(atom);
	      closeBox ())
        | ppExp'(HandleExp{expr,rules},atom,d) =
	     (openHVBox 0;
	       lpcond(atom);
	       ppExp'(expr,atom,d-1); newline ppstrm; pps "handle ";
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ())
	| ppExp' (RaiseExp exp,atom,d) = 
	      (openHVBox 0;
	       lpcond(atom);
	       pps "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ())
	| ppExp' (IfExp { test, thenCase, elseCase },atom,d) =
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
	| ppExp' (AndalsoExp (e1, e2),atom,d) =
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
	 | ppExp' (OrelseExp (e1, e2),atom,d) =
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
	 | ppExp' (WhileExp { test, expr },atom,d) =
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
	
	 | ppExp'(VectorExp nil,_,d) = pps "#[]"
	 | ppExp'(VectorExp exps,_,d) =
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
	 | ppExp'(MarkExp (exp,(s,e)),atom,d) =
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
             let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
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
				of RecordExp[(_,pl),(_,pr)] =>
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
		  | appPrint(AppExp{function=rator,argument=rand},l,r,d) =
		    (case stripMark rator
		       of VarExp v =>
			   let val path = v
			    in fixitypp(path,rand,l,r,d)
			   end
		        | rator =>
			   (openHOVBox 2;
			     ppExp'(rator,true,d-1); break ppstrm {nsp=1,offset=2};
			     ppExp'(rand,true,d-1);
			    closeBox ()))
		  | appPrint(MarkExp(exp,(s,e)),l,r,d) =
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
  in (fn(exp,depth)=> ppExp'(exp,false,depth))
  end
	
and ppRule (context as (env,source_opt)) ppstrm (Rule{pat,exp},d) =
    if d>0 then (openHVBox ppstrm (PP.Rel 0);
	  ppPat context ppstrm (pat,d-1);
	  PP.string ppstrm " =>"; break ppstrm {nsp=1,offset=2};
	  ppExp context ppstrm (exp,d-1);
	  closeBox ppstrm)
    else PP.string ppstrm "<rule>"

and ppStrExp (context as (_,source_opt)) ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
        val pp_symbol_list = pp_path ppstrm
        fun  ppStrExp'(_,0) = pps "<strexp>"

	   | ppStrExp'(VarStr p, d) = pp_symbol_list(p)

           | ppStrExp'(BaseStr(SeqDec nil), d) =
	       (pps "struct"; nbSpace ppstrm 1; pps "end")
           | ppStrExp'(BaseStr de, d) =
               (openVBox ppstrm (Rel 0);
                pps "stuct";  PU.nl_indent ppstrm 2;
                ppDec context ppstrm (de, d-1);
                pps "end";
                PP.closeBox ppstrm)
	   | ppStrExp'(ConstrainedStr (stre, constraint), d) =
               (openHOVBox 0;
                ppStrExp' (stre, d-1);
                case constraint
                  of NoSig => ()
                   | Transparent sigexp => 
                     (pps " :"; break ppstrm {nsp=1,offset=2};
                      ppSigExp context ppstrm (sigexp, d-1))
                   | Opaque sigexp => 
                     (pps " :>"; break ppstrm {nsp=1,offset=2};
                      ppSigExp context ppstrm (sigexp, d-1));
                closeBox ())
	   | ppStrExp'(AppStr (path, str_list), d) = 
	       let fun pr ppstrm (strl, bool) =
                       (pps "("; ppStrExp context ppstrm (strl,d); pps ")")
	        in pp_symbol_list(path); ppSequence ppstrm
		    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		     pr=pr,
		     style=INCONSISTENT}
		    str_list
               end	

           | ppStrExp'(AppStrI (path, str_list), d) = 
	       let fun pr ppstrm (strl, bool) =
                       (pps "("; ppStrExp context ppstrm (strl,d); pps ")")
	        in pp_symbol_list(path); ppSequence ppstrm
		    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		     pr=pr,
		     style=INCONSISTENT}
		    str_list
               end	

	   | ppStrExp'(LetStr(dec,body),d) =
	      (openHVBox 0;
	       pps "let "; ppDec context ppstrm (dec,d-1); 
               newline ppstrm;
	       pps " in "; ppStrExp'(body,d-1); newline ppstrm;
	       pps "end";
	       closeBox ())

         | ppStrExp'(MarkStr(body,(s,e)),d) =
             ppStrExp' (body,d)
(*	      (case source_opt
		of SOME source =>
	           (pps "MARKstr(";
		      ppStrExp'(body,d); pps ",";
		      prpos(ppstrm,source,s); pps ",";
		      prpos(ppstrm,source,e); pps ")")
	         | NONE => ppStrExp'(body,d))
*)
    in
	ppStrExp'
    end

and ppFctExp (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppFctExp'(_, 0) = pps "<fctexp>"
	  | ppFctExp'(VarFct (p, _), d) = pp_symbol_list(p)
          | ppFctExp'(LetFct(dec,body),d) =
	      (openHVBox ppstrm (PP.Rel 0);
	        pps "let "; ppDec context ppstrm (dec,d-1); 
                newline ppstrm;
	        pps " in "; ppFctExp'(body,d-1); newline ppstrm;
	        pps "end";
	       closeBox ppstrm)
	  | ppFctExp'(AppFct(path,sblist, fsigconst),d) =
              let fun pr ppstrm (strexp, _) =
                      (pps "("; ppStrExp context ppstrm (strexp,d); pps ")")
               in openHVBox ppstrm (PP.Rel 0);
                  pp_symbol_list path;
                  ppSequence ppstrm
		     {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		      pr = pr,
		      style = INCONSISTENT}
		     sblist;
                  closeBox ppstrm
              end
	  | ppFctExp'(MarkFct(body,(s,e)),d) =
	     ppFctExp' (body,d)
          | ppFctExp'(BaseFct _, d) = ErrorMsg.impossible "ppFctExp: BaseFct"
    in
	ppFctExp'
    end

and ppWhereSpec (context as (env,source_opt)) ppstrm =
  let val pps = PP.string ppstrm
      fun ppWhereSpec'(_,0) = pps "<WhereSpec>"
        | ppWhereSpec'(WhType([],[],ty),d) = ppTy context ppstrm (ty, d)
        | ppWhereSpec'(WhType(slist,tvlist,ty),d) = 
            let fun pr _ sym = ppSym ppstrm sym
                fun pr' _ tyv = ppTyvar context ppstrm (tyv,d)
	      in  pps "type "; ppSequence ppstrm
		  {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   pr=pr',
		   style=INCONSISTENT}
		   tvlist; break ppstrm {nsp=1,offset=0};
              ppSequence ppstrm
		  {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   pr=pr,
		   style=INCONSISTENT}
		   slist;	 
		   pps" ="; break ppstrm {nsp=1,offset=0}; ppTy context ppstrm (ty,d)
	      end
        | ppWhereSpec'(WhStruct(slist,slist'),d) =
	      let fun pr _ sym = ppSym ppstrm sym
            in pps "structure "; ppSequence ppstrm
                {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                 pr=pr,
                 style=INCONSISTENT}
                 slist;break ppstrm {nsp=1,offset=0};
                ppSequence ppstrm
                {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                 pr=pr,
                 style=INCONSISTENT}
                 slist'
             end
  in
      ppWhereSpec'
  end

and ppSigExp (context as (env,source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, newline, break} = en_pp ppstrm
      fun ppSigExp'(_,0) = pps "<SigExp>"
	| ppSigExp'(VarSig s,d) = (ppSym ppstrm s)
	| ppSigExp'(AugSig (sign, wherel),d) =
           (ppSigExp' (sign, d); break {nsp=1,offset=0};
	   (case sign
		of VarSig s => ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel)
		 | MarkSig(VarSig s, r) => ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel)
                 | _ => (newline ();  ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel))
		))
	| ppSigExp'(BaseSig [],d) =	
	    (pps "sig"; nbSpace ppstrm 1; pps"end")
	| ppSigExp'(BaseSig specl,d) = 
	  let fun pr ppstrm speci = (ppSpec context ppstrm (speci,d))
	   in (openVBox ppstrm (Rel 0);
                pps "sig"; PU.nl_indent ppstrm 2;
                ppSequence ppstrm
                 {sep=(fn ppstrm => (PP.newline ppstrm)),
                  pr=pr,
                  style=INCONSISTENT}
                 specl;
                newline (); pps"end";
               PP.closeBox ppstrm)
	  end
	| ppSigExp'(MarkSig (m,r),d) = ppSigExp context ppstrm (m,d)
  in
      ppSigExp'
  end

and ppFsigExp (context as (env,source_opt)) ppstrm =
  let val pps = PP.string ppstrm
      fun ppFsigExp'(_,0) = pps "<FsigExp>"
	| ppFsigExp'(VarFsig s,d) = ppSym ppstrm s
	| ppFsigExp'(BaseFsig {param,result},d) =
	  let fun pr ppstrm (SOME symbol, sigexp) =
		  (PU.pps ppstrm "("; ppSym ppstrm symbol; PU.pps ppstrm ":";
		   ppSigExp context ppstrm (sigexp, d);
                   PU.pps ppstrm ")")
		| pr ppstrm (NONE, sigexp) =
		  (PU.pps ppstrm "("; ppSigExp context ppstrm (sigexp, d);
                   PU.pps ppstrm ")")
	   in ppSequence ppstrm
               {sep=(fn ppstrm => (newline ppstrm)),
		pr = pr,
		style = INCONSISTENT}
	       param;
	      break ppstrm {nsp=1,offset=2};
	      pps "=> ";
	      ppSigExp context ppstrm (result,d)
	  end
		
	| ppFsigExp'(MarkFsig (m,r),d) = ppFsigExp context ppstrm (m,d)
  in
      ppFsigExp'
  end

and ppSpec (context as (env,source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
      fun pp_tyvar_list([],d) = ()
        | pp_tyvar_list([tyvar],d) = 
              (ppTyvar context ppstrm (tyvar,d); break ppstrm {nsp=1,offset=0})
        |  pp_tyvar_list (tyvar_list,d) = 
		let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar,d))
                in ppClosedSequence ppstrm
                    {front=(fn ppstrm => PP.string ppstrm "("),
                     sep=(fn ppstrm =>
                             (PP.string ppstrm ","; break ppstrm {nsp=1,offset=0})),
                     back=(fn ppstrm =>
                              (PP.string ppstrm ")"; break ppstrm {nsp=1,offset=0})),
                     pr=pr,
                     style=INCONSISTENT}
                    tyvar_list
                end
         
      fun ppSpec'(_,0) = pps "<Spec>"
	| ppSpec'(StrSpec sspo_list,d) =
	  let fun pr _ (symbol, sigexp, path) =
		  (case path
                    of SOME p => (ppSym ppstrm symbol; pps " = ";
                                  ppSigExp context ppstrm (sigexp,d);
                                  break ppstrm {nsp=1,offset=0}; pp_path ppstrm p)
                     | NONE => (ppSym ppstrm symbol; pps " = ";
                                ppSigExp context ppstrm (sigexp,d)))
	   in ppClosedSequence ppstrm
	       {front=(C PP.string "structure "),
		sep=(fn ppstrm => (PP.string ppstrm ",";
				   break ppstrm {nsp=1,offset=0})),
		back=(C PP.string ""),
		pr=pr,
		style=INCONSISTENT}
	       sspo_list
	  end 
	| ppSpec'(TycSpec (stto_list, bool),d) = 
	  let fun pr _ (symbol, tyvar_list, tyo) =
		  (case tyo
		    of SOME ty =>
                       (pp_tyvar_list (tyvar_list,d);ppSym ppstrm symbol; pps "= ";
                        ppTy context ppstrm(ty, d))
		     | NONE =>  (pp_tyvar_list (tyvar_list,d);ppSym ppstrm symbol))
	   in ppClosedSequence ppstrm
	        {front=(C PP.string "type "),
		 sep=(fn ppstrm => (PP.string ppstrm "|";newline ppstrm)),
		 back=(C PP.string ""),
		 pr=pr,
		 style=INCONSISTENT}
		stto_list
	  end 
	| ppSpec'(FctSpec sf_list,d) =
	  let fun pr ppstrm (symbol, fsigexp) =
                  (ppSym ppstrm symbol; pps " : ";
                   ppFsigExp context ppstrm (fsigexp, d-1)) 
	   in openHVBox 0;
              ppvlist ppstrm ("functor ", "and ", pr, sf_list);
              closeBox ()
	  end 
	| ppSpec'(ValSpec st_list,d) = 
	  let fun pr ppstrm (symbol, ty) = 
                  (ppSym ppstrm symbol; pps ":"; ppTy context ppstrm (ty, d)) 
	   in openHVBox 0;
              ppvlist ppstrm ("val ", "and ", pr, st_list);
              closeBox ()
	  end 
	| ppSpec'(DataSpec{datatycs,withtycs=[]},d) = 
	  let fun pr ppstrm (dbing) = (ppDb context ppstrm (dbing, d))
	   in openHVBox 0;
              ppvlist ppstrm ("datatype ", "and ", pr, datatycs);
              closeBox ()
	  end 
	| ppSpec'(DataSpec {datatycs, withtycs},d) = 
	  let fun prd ppstrm (dbing) = (ppDb context ppstrm (dbing, d))
	      fun prw ppstrm (tbing) = (ppTb context ppstrm (tbing, d))
	   in (openHVBox 0;
               ppvlist ppstrm ("datatype ", "and ", prd, datatycs);
	       newline ppstrm;
               ppvlist ppstrm ("datatype ", "and ", prw, withtycs);
	       closeBox ())
	  end
	| ppSpec'(ExceSpec sto_list,d) = 
	  let fun pr ppstrm (symbol, tyo) =
		  (case tyo
		    of SOME ty =>
                       (ppSym ppstrm symbol; pps " : ";
                        ppTy context ppstrm (ty, d))
		     | NONE =>  ppSym ppstrm symbol)
	   in openHVBox 0;
              ppvlist ppstrm ("exception ", "and ", pr, sto_list);
              closeBox ()
	  end 
	| ppSpec'(ShareStrSpec paths,d) = 
	   (openHVBox 0;
            ppvlist ppstrm ("sharing ", " = ", pp_path, paths);
            closeBox ())
        | ppSpec'(ShareTycSpec paths,d) = 
	   (openHVBox 0;
            ppvlist ppstrm ("sharing type ", " = ", pp_path, paths);
            closeBox ())
	| ppSpec'(IncludeSpec sigexp ,d) = ppSigExp context ppstrm (sigexp, d)
	| ppSpec'(MarkSpec (m,r),d) = ppSpec context ppstrm (m,d)
  in
      ppSpec'
  end

and ppDec (context as (env,source_opt)) ppstrm =
  let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
      val pp_symbol_list = pp_path ppstrm

      fun ppDec'(_,0) = pps "<dec>"
        | ppDec'(ValDec (vbs, tyvars),d) =
	  (openHVBox 0;
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVb context ppstrm (vb,d-1)),vbs);
	   closeBox ())
	| ppDec'(ValrecDec (rvbs, tyvars),d) = 
	  (openHVBox 0;
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRvb context ppstrm (rvb,d-1)),rvbs);
	   closeBox ())
        | ppDec'(FunDec (fbs,tyvars),d) =
	  (openHVBox 0;
	   ppvlist' ppstrm ("fun ","and ",
	     (fn ppstrm => fn str => fn fb => ppFb context ppstrm str (fb,d-1)),
             fbs);
	   closeBox ())
        | ppDec'(TypeDec tycs,d) =
	  let fun pr ppstrm (tyc) = (ppTb context ppstrm (tyc, d))
	   in ppClosedSequence ppstrm
		{front=(C PP.string "type "),
		 sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string ""),
		 pr=pr,
		 style=INCONSISTENT}
		tycs
	  end 	
	| ppDec'(DatatypeDec{datatycs,withtycs=[]},d) = 
	  let fun prd _ (dbing) = (ppDb context ppstrm (dbing, d))
	   in ppClosedSequence ppstrm
		{front=(C PP.string "datatype "),
		 sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string ""),
		 pr=prd,
		 style=INCONSISTENT}
		datatycs
	  end             
        | ppDec'(DatatypeDec{datatycs,withtycs},d) = 
	  let fun prd ppstrm dbing = (ppDb context ppstrm (dbing, d))
	      fun prw ppstrm tbing = (ppTb context ppstrm (tbing, d))
	   in (openHVBox 0;
	        ppClosedSequence ppstrm
		  {front=(C PP.string "datatype "),
		   sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   back=(C PP.string ""),
		   pr=prd,
		   style=INCONSISTENT}
		  datatycs;
	       newline ppstrm;
	       ppClosedSequence ppstrm
		 {front=(C PP.string "withtype "),
		  sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		  back=(C PP.string ""),
		  pr=prw,
		  style=INCONSISTENT}
		 withtycs;
	       closeBox ())
	  end
	| ppDec'(AbstypeDec{abstycs,withtycs=[],body},d) = 
	  let fun prd ppstrm dbing = (ppDb context ppstrm (dbing, d))
	      fun prw ppstrm tbing = (ppTb context ppstrm (tbing, d))
	   in (openHVBox 0;
	        (ppClosedSequence ppstrm
		   {front=(C PP.string "datatype "),
		    sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		    back=(C PP.string ""),
		    pr=prd,
		    style=INCONSISTENT}
		   abstycs);
	       newline ppstrm;
	       ppDec' (body, d);
	       closeBox ())
	  end
        | ppDec'(AbstypeDec{abstycs,withtycs,body},d) = 
	  let fun prd _ (dbing) = (ppDb context ppstrm (dbing, d))
	      fun prw _ (tbing) = (ppTb context ppstrm (tbing, d))
	   in (openHVBox 0;
	        (ppClosedSequence ppstrm
		   {front=(C PP.string "datatype "),
		    sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		    back=(C PP.string ""),
		    pr=prd,
		    style=INCONSISTENT}
		   abstycs);
	       newline ppstrm;
	       (ppClosedSequence ppstrm
		  {front=(C PP.string "withtype "),
		   sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   back=(C PP.string ""),
		   pr=prw,
		   style=INCONSISTENT}
		  withtycs);
	       newline ppstrm;
	       ppDec' (body, d);
	       closeBox ())
	  end
        | ppDec'(ExceptionDec ebs,d) =
	  (openHVBox 0;
	   ((fn ppstrm => fn eb => ppEb context ppstrm (eb,d-1)),ebs);
	   closeBox ())
        | ppDec'(StrDec sbs,d) =
          let fun pr _ (sbing) = (ppStrb context ppstrm (sbing, d))
		in ppClosedSequence ppstrm
		  {front=(C PP.string "structure "),
		   sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   back=(C PP.string ""),
		   pr=pr,
		   style=INCONSISTENT}
		   sbs
		end
        | ppDec'(AbsDec sbs,d) =
          let fun f ppstrm (Strb{name, def, constraint}) =
		    (ppSym ppstrm name;
		     break ppstrm {nsp=1,offset=2};
		     ppStrExp context ppstrm (def,d-1))
		| f _ _ = bug "ppDec':ABSdec"
	   in openHVBox 0;
	      ppvlist ppstrm ("abstraction ","and ", f, sbs);
	      closeBox ()
	  end
        | ppDec'(FctDec fbs,d) = 
	  let fun f ppstrm fctb = ppFctb context ppstrm (fctb,d)
	   in openHVBox 0;
	      ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ()
	  end
        | ppDec'(SigDec sigvars,d) = 
	  let fun f ppstrm (Sigb{name=fname, def}) =
                  (ppSym ppstrm fname; pps " =";
		   newline ppstrm;
		   ppSigExp context ppstrm (def,d))
		| f ppstrm (MarkSigb(t,r)) = f ppstrm t
	   in openHVBox 0;
	      ppvlist ppstrm ("signature ","and ", f, sigvars);
              closeBox ()
	  end
        | ppDec'(FsigDec sigvars,d) = 
	  let fun pr ppstrm sigv = ppFsigb context ppstrm (sigv,d)
	   in openHVBox 0;
	       ppSequence ppstrm
	         {sep=newline,
	          pr=pr,
	          style=CONSISTENT}
	         sigvars;
	      closeBox ()
	  end
        | ppDec'(LocalDec(inner,outer),d) =
	    (openHVBox 0;
	     pps "local"; nl_indent ppstrm 2;
	     ppDec'(inner,d-1); newline ppstrm;
	     pps "in ";
	     ppDec'(outer,d-1); newline ppstrm;
	     pps "end";
	     closeBox ())
        | ppDec'(SeqDec decs,d) =
	    (openHVBox 0;
	     ppSequence ppstrm
	       {sep=newline,
	        pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	        style=CONSISTENT}
	        decs;
	     closeBox ())
        | ppDec'(OpenDec strbs,d) = 
	  (openHVBox 0;
	   pps "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=(fn ppstrm => fn sp => pp_symbol_list sp),
	      style=INCONSISTENT}
             strbs;
	   closeBox ())
        | ppDec'(OvldDec (sym, ty, explist),d) = ppSym ppstrm sym
	| ppDec'(FixDec {fixity,ops},d) =
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

        | ppDec'(MarkDec(dec,(s,e)),d) =  
	   (case source_opt
	      of SOME source =>
	         (pps "MarkDec(";
		  ppDec'(dec,d); pps ",";
		  prpos(ppstrm,source,s); pps ",";
		  prpos(ppstrm,source,e); pps ")")
	       | NONE => ppDec'(dec,d))
	  
     in ppDec'
    end

and ppVb (context as (env,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppVb'(_,0)= pps "<binding>"
	  | ppVb'(Vb{pat,exp,...},d) = 
		(openHVBox ppstrm (PP.Rel 0);
	  	 ppPat context ppstrm (pat,d-1); PP.string ppstrm " =";
	  	 break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	  	 closeBox ppstrm)
	  | ppVb'(MarkVb (vb,region),d) = ppVb' (vb,d)
    in
	ppVb'
    end

and ppRvb (context as (_,source_opt)) ppstrm  = 
    let val pps = PP.string ppstrm
	fun ppRvb'(_,0)= pps "<rec binding>"
	  | ppRvb'(Rvb{var, exp, ...},d) =
	     (openHOVBox ppstrm (PP.Rel 0);
	      ppSym ppstrm var; PP.string ppstrm " =";
	      break ppstrm {nsp=1,offset=2}; ppExp context ppstrm (exp,d-1);
	      closeBox ppstrm)
	  | ppRvb'(MarkRvb (rvb, region), d) = ppRvb' (rvb, d)
    in
	ppRvb'
    end

and ppFb (context as (_,source_opt)) ppstrm head = 
    let val pps = PP.string ppstrm
	fun ppFb'(_,0)= pps "<FunBinding>"
	  | ppFb'(Fb (clauses, ops),d) =
              ppvlist ppstrm (head, "  | ",
	       (fn ppstrm => fn (cl: clause) => (ppClause context ppstrm (cl,d))),
               clauses)
	  | ppFb'(MarkFb (t,r),d) = ppFb context ppstrm head (t,d)
    in
	ppFb'
    end

and ppClause (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
        fun ppClause' (Clause{pats, resultty, exp}, d) =
	    let fun pr _ {item:pat,fixity:symbol option,region:region} =
		    (case fixity
		      of SOME a => ppPat context ppstrm (item,d)
		       | NONE => (
			 case item
			  of FlatAppPat p =>
			     (PP.string ppstrm "(";ppPat context ppstrm (item,d);
			      PP.string ppstrm ")")
			   | ConstraintPat p  =>
			     (PP.string ppstrm "(";ppPat context ppstrm (item,d);
			      PP.string ppstrm ")")
			   | LayeredPat p =>
			     (PP.string ppstrm"(";ppPat context ppstrm (item,d);
			      PP.string ppstrm ")")
			   | OrPat p =>
			     (PP.string ppstrm "(";ppPat context ppstrm (item,d);
			      PP.string ppstrm ")")
			   | _ => ppPat context ppstrm (item,d)))
				 
		in (openHOVBox ppstrm (PP.Rel 0);
	  	      (ppSequence ppstrm
			{sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
			 pr=pr,
			 style=INCONSISTENT}
			pats);
		    (case resultty
		      of SOME ty => (PP.string ppstrm ":";ppTy context ppstrm (ty,d))
		       | NONE => ()
		    );
		    PP.string ppstrm " =";
		    break ppstrm {nsp=1,offset=0}; 
		    ppExp context ppstrm (exp,d);
	  	    closeBox ppstrm)	
		end 
	  	    
    in
	ppClause'
    end

and ppTb (context as (_,source_opt)) ppstrm  = 
    let val pps = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar, d))
	     in ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm "*";
                                     break ppstrm {nsp=1,offset=0})),
		  pr=pr,
		  style=INCONSISTENT}
		 symbol_list
	    end

	  fun ppTb'(_,0)= pps "<T.binding>"
	    | ppTb'(Tb{tyc,def,tyvars},d) = 
		(openHOVBox ppstrm (PP.Rel 0);
	  	  ppSym ppstrm tyc; PP.string ppstrm " =";
	  	  break ppstrm {nsp=1,offset=0}; ppTy context ppstrm (def, d);
		  pp_tyvar_list (tyvars,d);
	  	 closeBox ppstrm)
	    | ppTb'(MarkTb (t,_,_),d) = ppTb context ppstrm (t,d)
    in
	ppTb'
    end

and ppDb (context as (_,source_opt)) ppstrm  = 
    let val pps = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar, d))
	     in ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm "*";
				     break ppstrm {nsp=1,offset=0})),
		  pr=pr,
		  style=INCONSISTENT}
		 symbol_list
	    end
	fun ppDb'(_,0)= pps "<D.binding>"
	  | ppDb'(Db{tyc,tyvars,rhs,lazyp},d) = 
	     (openHOVBox ppstrm (PP.Rel 0);
	      ppSym ppstrm tyc; PP.string ppstrm " =";
	      break ppstrm {nsp=1,offset=0}; ppDbrhs context ppstrm (rhs,d);
	      closeBox ppstrm)
	  | ppDb'(MarkDb (t,r),d) = ppDb context ppstrm (t,d)
    in
	ppDb'
    end

and ppDbrhs (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppDbrhs'(_,0)= pps "<dbrhs>"
	  | ppDbrhs'(Constrs const,d) = 
	    let fun pr ppstrm (sym:symbol, tv:Ast.ty option) =
		    (case tv
		       of SOME a =>
			  (ppSym ppstrm sym; pps" of "; ppTy context ppstrm (a, d))
		        | NONE =>
			  (ppSym ppstrm sym))
	        in  ppSequence ppstrm
		    {sep=(fn ppstrm => (PP.string ppstrm " |";
					break ppstrm {nsp=1,offset=0})),
		     pr=pr,
		     style=INCONSISTENT}
		     const
	        end
	  | ppDbrhs'(Repl symlist,d) = 
	     ppSequence ppstrm
              {sep=(fn ppstrm => (PP.string ppstrm " |";
                                  break ppstrm {nsp=1,offset=0})),
               pr=(fn ppstrm => fn sym => ppSym ppstrm sym),
               style=INCONSISTENT}
               symlist
     in ppDbrhs'
    end

and ppEb (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppEb'(_,0)= pps "<Eb>"
	  | ppEb'(EbGen{exn, etype},d) = 
	     (case etype
	        of SOME a => 
		   (openHVBox ppstrm (PP.Rel 0);
	  	    ppSym ppstrm exn; PP.string ppstrm " =";
	  	    break ppstrm {nsp=1,offset=2}; ppTy context ppstrm (a,d-1);
	  	    closeBox ppstrm)
		 | NONE =>
		   (openHVBox ppstrm (PP.Rel 0);
	  	    ppSym ppstrm exn; 
	  	    closeBox ppstrm))
	  | ppEb'(EbDef{exn, edef},d) = 
		(*ASK MACQUEEN IF WE NEED TO PRINT EDEF*)
	     (openHVBox ppstrm (PP.Rel 0);
	      ppSym ppstrm exn; PP.string ppstrm " =";
	      break ppstrm {nsp=1,offset=2}; pp_symbol_list(edef);
	      closeBox ppstrm)
	  | ppEb'(MarkEb (t,r),d) = ppEb context ppstrm (t,d)
    in
	ppEb'
    end

and ppStrb (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppStrb'(_,0)= pps "<Strb>"
	  | ppStrb'(Strb{name,def,constraint},d) = 
	     (openHVBox ppstrm (PP.Rel 0);
	      ppSym ppstrm name; PP.string ppstrm " :";
	      break ppstrm {nsp=1,offset=2}; ppStrExp context ppstrm (def,d-1);
	      closeBox ppstrm)
	  | ppStrb'(MarkStrb (t,r),d) = ppStrb context ppstrm (t,d)
    in
	ppStrb'
    end

and ppFctb (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppFctb'(_,0)= pps "<Fctb>"
	  | ppFctb'(Fctb{name,def=BaseFct{params,body,constraint}},d) =
	    (openHVBox ppstrm (PP.Rel 0);
             ppSym ppstrm name;
             let fun pr ppstrm (SOME symbol, sigexp) =
                     (pps "("; ppSym ppstrm symbol; pps " : ";
                      ppSigExp context ppstrm (sigexp, d); pps ")")
                   | pr ppstrm (NONE, sigexp) =
                     (pps "("; ppSigExp context ppstrm (sigexp, d); pps ")")
              in (ppSequence ppstrm
                    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                     pr = pr,
                     style = INCONSISTENT}
                    params;
                  case constraint
                    of NoSig => ()
                     | Transparent(sigexp) => 
                       (pps " :"; break ppstrm {nsp=1,offset=2};
                        ppSigExp context ppstrm (sigexp,d))
                     | Opaque(sigexp) => 
                       (pps " :>"; break ppstrm {nsp=1,offset=2};
                        ppSigExp context ppstrm (sigexp,d));
                  nbSpace ppstrm 1;
                  pps "="; break ppstrm {nsp=1,offset=0};
                  ppStrExp context ppstrm (body,d))
              end;
             closeBox ppstrm)
	  | ppFctb'(Fctb{name,def},d) =
	    (openHVBox ppstrm (PP.Rel 0);
	     ppSym ppstrm name; PP.string ppstrm " =";
	     break ppstrm {nsp=1,offset=2}; ppFctExp context ppstrm (def,d-1);
	     closeBox ppstrm) 
	  | ppFctb'(MarkFctb (t,r),d) = ppFctb context ppstrm (t,d)
    in
	ppFctb'
    end

and ppFsigb (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppFsigb'(_,0)= pps "<Fsigb>"
	  | ppFsigb'(Fsigb{name,def},d) = 
	    (openHVBox ppstrm (PP.Rel 0);
	      pps "funsig "; ppSym ppstrm name; pps " =";
	      break ppstrm {nsp=1,offset=2}; ppFsigExp context ppstrm (def,d-1);
	     closeBox ppstrm)
	  | ppFsigb'(MarkFsigb (t,r),d) = ppFsigb context ppstrm (t,d)
    in
	ppFsigb'
    end

and ppTyvar (context as (_,source_opt)) ppstrm =
    let val pps = PP.string ppstrm
	fun ppTyvar'(_,0)= pps "<tyvar>"
	  | ppTyvar'(Tyv s,d) = (ppSym ppstrm s) 
	  | ppTyvar'(MarkTyv (t,r),d) = ppTyvar context ppstrm (t,d)
    in
	ppTyvar'
    end

and ppTy (context as (env, source_opt)) ppstrm =			
  let val pps = PP.string ppstrm
      fun ppTy' (_,0) = pps "<type>"
        | ppTy' (VarTy t,d) =  (ppTyvar context ppstrm (t,d))
	| ppTy' (ConTy (tycon, []),d) =
	  (openHVBox ppstrm (PP.Rel 1);
	   pp_path ppstrm tycon;
	   closeBox ppstrm)
	| ppTy' (ConTy (tycon, args),d) = 
	  (openHVBox ppstrm (PP.Rel 1);
           case tycon
             of [tyc] =>
	         if S.eq(S.tycSymbol("->"), tyc) then
                   (case args
                      of [dom,ran] =>
                         (ppTy' (dom,d-1); pps " ->"; break ppstrm {nsp=1,offset=2};
                          ppTy' (ran,d-1))
                       | _ => EM.impossible "wrong args for -> type")
		 else (ppTypeArgs(args,d);
	               ppSym ppstrm tyc;
	               closeBox ppstrm)
              | _ => (ppTypeArgs(args,d);
	              pp_path ppstrm tycon;
	              closeBox ppstrm))

	| ppTy' (RecordTy s, d) = 
	  let fun pr ppstrm (sym:symbol, tv:Ast.ty) = 
		  (ppSym ppstrm sym; pps ":"; ppTy context ppstrm (tv, d))
	  in  ppClosedSequence ppstrm
	        {front=(C PP.string "{"),
		 sep=(fn ppstrm => (PP.string ppstrm ",";
				    break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string "}"),
		 pr=pr,
		 style=INCONSISTENT}
		s
	  end
	| ppTy' (TupleTy t, d) = 
	  let fun pr _ (tv:Ast.ty) = (ppTy context ppstrm (tv, d))
	  in  ppSequence ppstrm
	       {sep=(fn ppstrm => (PP.string ppstrm " *";
		                   break ppstrm {nsp=1,offset=0})),
		pr=pr,
		style=INCONSISTENT}
	       t
	  end
	| ppTy' (MarkTy (t,r),d) = (ppTy context ppstrm (t,d))
      and ppTypeArgs ([],d) = ()
	| ppTypeArgs ([ty],d) = 
	  (if strength ty <= 1
	   then (openHOVBox ppstrm (PP.Rel 1);
                 pps "("; 
                 ppTy' (ty,d); 
                 pps ")";
                 closeBox ppstrm)
	   else ppTy' (ty,d);
	   break ppstrm {nsp=1,offset=0})
	| ppTypeArgs (tys,d) =
          ppClosedSequence ppstrm 
	   {front=C PP.string "(",
	    sep=fn ppstrm => (PP.string ppstrm ",";
                              break ppstrm {nsp=0,offset=0}),
		   back=C PP.string ") ",
		   style=INCONSISTENT, 
                   pr=fn _ => fn ty => ppTy' (ty,d)}
	   tys 
   in ppTy'
  end

end (* top-level local *)
end (* structure PPAst *)

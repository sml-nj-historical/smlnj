(* Copyright 2003 by University of Chicago *)
(* Jing Cao and Lukasz Ziarek *)

structure PPAst: PPAST = 
struct

local structure EM = ErrorMsg
      structure M = Modules
      structure B = Bindings
      structure S = Symbol
      structure BT = BasicTypes
      structure PP = PrettyPrint

      open Ast Tuples Fixity VarCon Types PrettyPrint PPUtil PPType PPVal
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

fun prpos(ppstrm: PrettyPrint.stream,
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
    let val ppsay = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppPat' (WildPat,_) = (ppsay "_")
	  | ppPat' (VarPat p, d) =  pp_symbol_list(p)
	  | ppPat' (IntPat i,_) = ppsay(IntInf.toString i)
	  | ppPat' (WordPat w,_) = ppsay(IntInf.toString w)
	  | ppPat' (StringPat s, _) = pp_mlstr ppstrm s
	  | ppPat' (CharPat s,_) = (ppsay "#"; pp_mlstr ppstrm s)
	  | ppPat' (LayeredPat {varPat,expPat},d) =
		(openHVBox ppstrm (PP.Rel 0);
		 ppPat'(varPat,d); ppsay " as "; ppPat'(expPat,d-1);
		 closeBox ppstrm)
	  | ppPat' (RecordPat{def=[],flexibility},_) =
		if flexibility then ppsay "{...}"
		else ppsay "()"
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
	  | ppPat' (ListPat nil, d) = ppsay "[]"
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
		(openHVBox ppstrm (PP.Rel 0);
		 ppPat'(constr,d); ppsay " as "; ppPat'(argument,d);
		 closeBox ppstrm)
	  | ppPat' (ConstraintPat {pattern, constraint}, d) = 
		(openHOVBox ppstrm (PP.Rel 0);
		 ppPat' (pattern, d-1); ppsay " :";
		 break ppstrm {nsp=1,offset=2};
		 ppTy context ppstrm (constraint, d);
		 closeBox ppstrm)
	  | ppPat' (VectorPat nil, d) = ppsay "#[]"
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
		     then (ppsay "<MARK(";
			   prpos(ppstrm,source,s); ppsay ",";
			   prpos(ppstrm,source,e); ppsay "): ";
			   ppPat'(pat,d); ppsay ">")
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
  let val ppsay = PP.string ppstrm
      fun lparen() = ppsay "(" 
      fun rparen() = ppsay ")"
      fun lpcond(atom) = if atom then ppsay "(" else ()      
      fun rpcond(atom) = if atom then ppsay ")" else ()
      val pp_symbol_list = pp_path ppstrm
      fun ppExp' (_,_,0) = ppsay "<exp>"
	| ppExp' (VarExp p,_,_) = pp_symbol_list(p)
	| ppExp' (FnExp nil,_,d) = ppsay "<function>"
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
	      (openHVBox ppstrm (PP.Rel 0);
	       ppsay "(case "; ppExp'(expr,true,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), 
                  trim rules);
	       rparen();
	       closeBox ppstrm)
	| ppExp' (LetExp {dec, expr},_,d) =
	      (openHVBox ppstrm (PP.Rel 0);
		ppsay "let ";
		openHVBox ppstrm (PP.Rel 0);
		 ppDec context ppstrm (dec,d-1); 
		closeBox ppstrm;
		break ppstrm {nsp=1,offset=0};
		ppsay "in ";
		openHVBox ppstrm (PP.Rel 0);
		 ppExp'(expr,false,d-1);
		closeBox ppstrm;
		break ppstrm {nsp=1,offset=0};
		ppsay "end";
	       closeBox ppstrm)
 	| ppExp'(SeqExp exps,_,d) =
	      ppClosedSequence ppstrm
	        {front=(C PP.string "("),
		 sep=(fn ppstrm => (PP.string ppstrm ";";
				    break ppstrm {nsp=1,offset=0})),
		 back=(C PP.string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,false,d-1)),
		 style=INCONSISTENT}
		exps
	| ppExp' (IntExp i,_,_) = ppsay (IntInf.toString i)
	| ppExp' (WordExp w,_,_) = ppsay (IntInf.toString w)
	| ppExp' (RealExp r,_,_) = ppsay r
	| ppExp' (StringExp s,_,_) = pp_mlstr ppstrm s
	| ppExp' (CharExp s,_,_) = (ppsay "#"; pp_mlstr ppstrm s)
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
			  (ppSym ppstrm name; ppsay "=";
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
	      (openHVBox ppstrm (PP.Rel 0);
	        lpcond(atom);
	        ppsay "#"; ppSym ppstrm name;
	        ppsay ">";
		rpcond(atom);
	       closeBox ppstrm)
	| ppExp' (ConstraintExp {expr,constraint},atom,d) = 
	     (openHOVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppExp'(expr,false,d); ppsay ":";
	       break ppstrm {nsp=1,offset=2};
	       ppTy context ppstrm (constraint,d);
	       rpcond(atom);
	      closeBox ppstrm)
        | ppExp'(HandleExp{expr,rules},atom,d) =
	     (openHVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppExp'(expr,atom,d-1); newline ppstrm; ppsay "handle ";
	       nl_indent ppstrm 2;
	       ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	       rpcond(atom);
	      closeBox ppstrm)
	| ppExp' (RaiseExp exp,atom,d) = 
	      (openHVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppsay "raise "; ppExp'(exp,true,d-1);
	       rpcond(atom);
	       closeBox ppstrm)
	| ppExp' (IfExp { test, thenCase, elseCase },atom,d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       ppsay "if ";
	       openHVBox ppstrm (PP.Rel 0);
	        ppExp' (test, false, d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "then ";
	       openHVBox ppstrm (PP.Rel 0);
	        ppExp' (thenCase, false, d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "else ";
	       openHVBox ppstrm (PP.Rel 0);
	        ppExp' (elseCase, false, d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	| ppExp' (AndalsoExp (e1, e2),atom,d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       openHVBox ppstrm (PP.Rel 0);
	       ppExp' (e1,true,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "andalso ";
	       openHVBox ppstrm (PP.Rel 0);
	       ppExp' (e2,true,d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	 | ppExp' (OrelseExp (e1, e2),atom,d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       lpcond(atom);
	       openHVBox ppstrm (PP.Rel 0);
	       ppExp' (e1,true,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "orelse ";
	       openHVBox ppstrm (PP.Rel 0);
	       ppExp' (e2,true,d-1);
	       closeBox ppstrm;
	       rpcond(atom);
	       closeBox ppstrm)
	 | ppExp' (WhileExp { test, expr },atom,d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       ppsay "while ";
	       openHVBox ppstrm (PP.Rel 0);
	        ppExp'(test,false,d-1);
	       closeBox ppstrm;
	       break ppstrm {nsp=1,offset= 0};
	       ppsay "do ";
	       openHVBox ppstrm (PP.Rel 0);
	         ppExp'(expr,false,d-1);
	       closeBox ppstrm;
	       closeBox ppstrm)
	
	 | ppExp'(VectorExp nil,_,d) = ppsay "#[]"
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
	 | ppExp' (StructurePluginExp { str, sgn },_,_) =
	     (ppsay "(structure ";
	      pp_symbol_list str;
	      ppsay ": ";
	      ppSym ppstrm sgn;
	      ppsay ")")
	     
	 | ppExp'(MarkExp (exp,(s,e)),atom,d) =
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
			    (openHOVBox ppstrm (PP.Rel 2);
			     ppsay dname; break ppstrm {nsp=1,offset=0};
			     ppExp'(exp,true,d-1);
			     closeBox ppstrm)
		     in case thisFix
			  of INfix _ =>
			     (case stripMark rand
				of RecordExp[(_,pl),(_,pr)] =>
				    let val atom = strongerL(leftFix,thisFix)
					     orelse strongerR(thisFix,rightFix)
					val (left,right) =
					    if atom then (nullFix,nullFix)
					    else (leftFix,rightFix)
				     in (openHOVBox ppstrm (PP.Rel 2);
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
		  | appPrint(AppExp{function=rator,argument=rand},l,r,d) =
		    (case stripMark rator
		       of VarExp v =>
			   let val path = v
			    in fixitypp(path,rand,l,r,d)
			   end
		        | rator =>
			   (openHOVBox ppstrm (PP.Rel 2);
			     ppExp'(rator,true,d-1); break ppstrm {nsp=1,offset=2};
			     ppExp'(rand,true,d-1);
			    closeBox ppstrm))
		  | appPrint(MarkExp(exp,(s,e)),l,r,d) =
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
    let val ppsay = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
        fun  ppStrExp'(_,0) = ppsay "<strexp>"

	   | ppStrExp'(VarStr p, d) = pp_symbol_list(p)

           | ppStrExp'(BaseStr(SeqDec nil), d) =
	       (ppsay "struct"; nbSpace ppstrm 1; ppsay "end")
           | ppStrExp'(BaseStr de, d) =
               (openVBox ppstrm (Rel 0);
                ppsay "stuct";  PPUtil.nl_indent ppstrm 2;
                ppDec context ppstrm (de, d-1);
                ppsay "end";
                closeBox ppstrm)
	   | ppStrExp'(ConstrainedStr (stre, constraint), d) =
               (openHOVBox ppstrm (Rel 0);
                ppStrExp' (stre, d-1);
                case constraint
                  of NoSig => ()
                   | Transparent sigexp => 
                     (ppsay " :"; break ppstrm {nsp=1,offset=2};
                      ppSigExp context ppstrm (sigexp, d-1))
                   | Opaque sigexp => 
                     (ppsay " :>"; break ppstrm {nsp=1,offset=2};
                      ppSigExp context ppstrm (sigexp, d-1));
                closeBox ppstrm)
	   | ppStrExp'(AppStr (path, str_list), d) = 
	       let fun pr ppstrm (strl, bool) =
                       (ppsay "("; ppStrExp context ppstrm (strl,d); ppsay ")")
	        in pp_symbol_list(path); ppSequence ppstrm
		    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		     pr=pr,
		     style=INCONSISTENT}
		    str_list
               end	

           | ppStrExp'(AppStrI (path, str_list), d) = 
	       let fun pr ppstrm (strl, bool) =
                       (ppsay "("; ppStrExp context ppstrm (strl,d); ppsay ")")
	        in pp_symbol_list(path); ppSequence ppstrm
		    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		     pr=pr,
		     style=INCONSISTENT}
		    str_list
               end	

	   | ppStrExp'(LetStr(dec,body),d) =
	      (openHVBox ppstrm (PP.Rel 0);
	       ppsay "let "; ppDec context ppstrm (dec,d-1); 
               newline ppstrm;
	       ppsay " in "; ppStrExp'(body,d-1); newline ppstrm;
	       ppsay "end";
	       closeBox ppstrm)

	   | ppStrExp'(PluginStr _,d) =
	       ppsay "<plugin>" (* FIXME *)

         | ppStrExp'(MarkStr(body,(s,e)),d) =
             ppStrExp' (body,d)
(*	      (case source_opt
		of SOME source =>
	           (ppsay "MARKstr(";
		      ppStrExp'(body,d); ppsay ",";
		      prpos(ppstrm,source,s); ppsay ",";
		      prpos(ppstrm,source,e); ppsay ")")
	         | NONE => ppStrExp'(body,d))
*)
    in
	ppStrExp'
    end

and ppFctExp (context as (_,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppFctExp'(_, 0) = ppsay "<fctexp>"
	  | ppFctExp'(VarFct (p, _), d) = pp_symbol_list(p)
          | ppFctExp'(LetFct(dec,body),d) =
	      (openHVBox ppstrm (PP.Rel 0);
	        ppsay "let "; ppDec context ppstrm (dec,d-1); 
                newline ppstrm;
	        ppsay " in "; ppFctExp'(body,d-1); newline ppstrm;
	        ppsay "end";
	       closeBox ppstrm)
	  | ppFctExp'(AppFct(path,sblist, fsigconst),d) =
              let fun pr ppstrm (strexp, _) =
                      (ppsay "("; ppStrExp context ppstrm (strexp,d); ppsay ")")
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
  let val ppsay = PP.string ppstrm
      fun ppWhereSpec'(_,0) = ppsay "<WhereSpec>"
        | ppWhereSpec'(WhType([],[],ty),d) = ppTy context ppstrm (ty, d)
        | ppWhereSpec'(WhType(slist,tvlist,ty),d) = 
            let fun pr _ sym = ppSym ppstrm sym
                fun pr' _ tyv = ppTyvar context ppstrm (tyv,d)
	      in  ppsay "type "; ppSequence ppstrm
		  {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   pr=pr',
		   style=INCONSISTENT}
		   tvlist; break ppstrm {nsp=1,offset=0};
              ppSequence ppstrm
		  {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		   pr=pr,
		   style=INCONSISTENT}
		   slist;	 
		   ppsay" ="; break ppstrm {nsp=1,offset=0}; ppTy context ppstrm (ty,d)
	      end
        | ppWhereSpec'(WhStruct(slist,slist'),d) =
	      let fun pr _ sym = ppSym ppstrm sym
            in ppsay "structure "; ppSequence ppstrm
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
  let val ppsay = PP.string ppstrm
      fun ppSigExp'(_,0) = ppsay "<SigExp>"
	| ppSigExp'(VarSig s,d) = (ppSym ppstrm s)
	| ppSigExp'(AugSig (sign, wherel),d) =
           (ppSigExp' (sign, d); break ppstrm {nsp=1,offset=0};
	   (case sign
		of VarSig s => ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel)
		 | MarkSig(VarSig s, r) => ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel)
                 | _ => (newline ppstrm;  ppvlist ppstrm ("where ","and ",
		 (fn ppstrm => fn r => ppWhereSpec context ppstrm (r,d-1)),wherel))
		))
	| ppSigExp'(BaseSig [],d) =	
	    (ppsay "sig"; nbSpace ppstrm 1; ppsay"end")
	| ppSigExp'(BaseSig specl,d) = 
	  let fun pr ppstrm speci = (ppSpec context ppstrm (speci,d))
	   in (openVBox ppstrm (Rel 0); ppsay "sig"; PPUtil.nl_indent ppstrm 2;
              ppSequence ppstrm
		{sep=(fn ppstrm => (newline ppstrm)),
		 pr=pr,
		 style=INCONSISTENT}
		specl;
              newline ppstrm; ppsay"end"; closeBox ppstrm)
	  end
	| ppSigExp'(MarkSig (m,r),d) = ppSigExp context ppstrm (m,d)
  in
      ppSigExp'
  end

and ppFsigExp (context as (env,source_opt)) ppstrm =
  let val ppsay = PP.string ppstrm
      fun ppFsigExp'(_,0) = ppsay "<FsigExp>"
	| ppFsigExp'(VarFsig s,d) = ppSym ppstrm s
	| ppFsigExp'(BaseFsig {param,result},d) =
	  let fun pr ppstrm (SOME symbol, sigexp) =
		  (ppsay "("; ppSym ppstrm symbol; ppsay ":";
		   ppSigExp context ppstrm (sigexp, d);
                   ppsay ")")
		| pr ppstrm (NONE, sigexp) =
		  (ppsay "("; ppSigExp context ppstrm (sigexp, d); ppsay ")")
	   in ppSequence ppstrm
               {sep=(fn ppstrm => (newline ppstrm)),
		pr = pr,
		style = INCONSISTENT}
	       param;
	      break ppstrm {nsp=1,offset=2};
	      ppsay "=> ";
	      ppSigExp context ppstrm (result,d)
	  end
		
	| ppFsigExp'(MarkFsig (m,r),d) = ppFsigExp context ppstrm (m,d)
  in
      ppFsigExp'
  end

and ppSpec (context as (env,source_opt)) ppstrm =
  let val ppsay = PP.string ppstrm
      fun pp_tyvar_list([],d) = ()
        | pp_tyvar_list([tyvar],d) = 
              (ppTyvar context ppstrm (tyvar,d); break ppstrm {nsp=1,offset=0})
        |  pp_tyvar_list (tyvar_list,d) = 
		let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar,d))
                in ppClosedSequence ppstrm
                    {front=(fn ppstrm => PP.string ppstrm "("),
                     sep=(PP.string ppstrm ",";fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                     back=(PP.string ppstrm ")";fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                     pr=pr,
                     style=INCONSISTENT}
                    tyvar_list
                end
         
      fun ppSpec'(_,0) = ppsay "<Spec>"
	| ppSpec'(StrSpec sspo_list,d) =
	  let fun pr _ (symbol, sigexp, path) =
		  (case path
                    of SOME p => (ppSym ppstrm symbol; ppsay " = ";
                                  ppSigExp context ppstrm (sigexp,d);
                                  break ppstrm {nsp=1,offset=0}; pp_path ppstrm p)
                     | NONE => (ppSym ppstrm symbol; ppsay " = ";
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
                       (pp_tyvar_list (tyvar_list,d);ppSym ppstrm symbol; ppsay "= ";
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
                  (ppSym ppstrm symbol; ppsay " : ";
                   ppFsigExp context ppstrm (fsigexp, d-1)) 
	   in openHVBox ppstrm (Rel 0);
              ppvlist ppstrm ("functor ", "and ", pr, sf_list);
              closeBox ppstrm
	  end 
	| ppSpec'(ValSpec st_list,d) = 
	  let fun pr ppstrm (symbol, ty) = 
                  (ppSym ppstrm symbol; ppsay ":"; ppTy context ppstrm (ty, d)) 
	   in openHVBox ppstrm (Rel 0);
              ppvlist ppstrm ("val ", "and ", pr, st_list);
              closeBox ppstrm
	  end 
	| ppSpec'(DataSpec{datatycs,withtycs=[]},d) = 
	  let fun pr ppstrm (dbing) = (ppDb context ppstrm (dbing, d))
	   in openHVBox ppstrm (Rel 0);
              ppvlist ppstrm ("datatype ", "and ", pr, datatycs);
              closeBox ppstrm
	  end 
	| ppSpec'(DataSpec {datatycs, withtycs},d) = 
	  let fun prd ppstrm (dbing) = (ppDb context ppstrm (dbing, d))
	      fun prw ppstrm (tbing) = (ppTb context ppstrm (tbing, d))
	   in (openHVBox ppstrm (PP.Rel 0);
               ppvlist ppstrm ("datatype ", "and ", prd, datatycs);
	       newline ppstrm;
               ppvlist ppstrm ("datatype ", "and ", prw, withtycs);
	       closeBox ppstrm)
	  end
	| ppSpec'(ExceSpec sto_list,d) = 
	  let fun pr ppstrm (symbol, tyo) =
		  (case tyo
		    of SOME ty =>
                       (ppSym ppstrm symbol; ppsay " : ";
                        ppTy context ppstrm (ty, d))
		     | NONE =>  ppSym ppstrm symbol)
	   in openHVBox ppstrm (Rel 0);
              ppvlist ppstrm ("exception ", "and ", pr, sto_list);
              closeBox ppstrm
	  end 
	| ppSpec'(ShareStrSpec paths,d) = 
	   (openHVBox ppstrm (Rel 0);
            ppvlist ppstrm ("sharing ", " = ", pp_path, paths);
            closeBox ppstrm)
        | ppSpec'(ShareTycSpec paths,d) = 
	   (openHVBox ppstrm (Rel 0);
            ppvlist ppstrm ("sharing type ", " = ", pp_path, paths);
            closeBox ppstrm)
	| ppSpec'(IncludeSpec sigexp ,d) = ppSigExp context ppstrm (sigexp, d)
	| ppSpec'(MarkSpec (m,r),d) = ppSpec context ppstrm (m,d)
  in
      ppSpec'
  end

and ppDec (context as (env,source_opt)) ppstrm =
  let val ppsay = PP.string ppstrm
      val pp_symbol_list = pp_path ppstrm

      fun ppDec'(_,0) = ppsay "<dec>"
        | ppDec'(ValDec (vbs, tyvars),d) =
	  (openHVBox ppstrm (PP.Rel 0);
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVb context ppstrm (vb,d-1)),vbs);
	   closeBox ppstrm)
	| ppDec'(ValrecDec (rvbs, tyvars),d) = 
	  (openHVBox ppstrm (PP.Rel 0);
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRvb context ppstrm (rvb,d-1)),rvbs);
	   closeBox ppstrm)
        | ppDec'(FunDec (fbs,tyvars),d) =
	  (openHVBox ppstrm (PP.Rel 0);
	   ppvlist' ppstrm ("fun ","and ",
	     (fn ppstrm => fn str => fn fb => ppFb context ppstrm str (fb,d-1)),
             fbs);
	   closeBox ppstrm)
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
	   in (openHVBox ppstrm (PP.Rel 0);
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
	       closeBox ppstrm)
	  end
	| ppDec'(AbstypeDec{abstycs,withtycs=[],body},d) = 
	  let fun prd ppstrm dbing = (ppDb context ppstrm (dbing, d))
	      fun prw ppstrm tbing = (ppTb context ppstrm (tbing, d))
	   in (openHVBox ppstrm (PP.Rel 0);
	        (ppClosedSequence ppstrm
		   {front=(C PP.string "datatype "),
		    sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
		    back=(C PP.string ""),
		    pr=prd,
		    style=INCONSISTENT}
		   abstycs);
	       newline ppstrm;
	       ppDec' (body, d);
	       closeBox ppstrm)
	  end
        | ppDec'(AbstypeDec{abstycs,withtycs,body},d) = 
	  let fun prd _ (dbing) = (ppDb context ppstrm (dbing, d))
	      fun prw _ (tbing) = (ppTb context ppstrm (tbing, d))
	   in (openHVBox ppstrm (PP.Rel 0);
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
	       closeBox ppstrm)
	  end
        | ppDec'(ExceptionDec ebs,d) =
	  (openHVBox ppstrm (PP.Rel 0);
	   ((fn ppstrm => fn eb => ppEb context ppstrm (eb,d-1)),ebs);
	   closeBox ppstrm)
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
	   in openHVBox ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("abstraction ","and ", f, sbs);
	      closeBox ppstrm
	  end
        | ppDec'(FctDec fbs,d) = 
	  let fun f ppstrm fctb = ppFctb context ppstrm (fctb,d)
	   in openHVBox ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("functor ","and ", f, fbs);
              closeBox ppstrm
	  end
        | ppDec'(SigDec sigvars,d) = 
	  let fun f ppstrm (Sigb{name=fname, def}) =
                  (ppSym ppstrm fname; ppsay " =";
		   newline ppstrm;
		   ppSigExp context ppstrm (def,d))
		| f ppstrm (MarkSigb(t,r)) = f ppstrm t
	   in openHVBox ppstrm (PP.Rel 0);
	      ppvlist ppstrm ("signature ","and ", f, sigvars);
              closeBox ppstrm
	  end
        | ppDec'(FsigDec sigvars,d) = 
	  let fun pr ppstrm sigv = ppFsigb context ppstrm (sigv,d)
	   in openHVBox ppstrm (PP.Rel 0);
	       ppSequence ppstrm
	         {sep=newline,
	          pr=pr,
	          style=CONSISTENT}
	         sigvars;
	      closeBox ppstrm
	  end
        | ppDec'(LocalDec(inner,outer),d) =
	    (openHVBox ppstrm (PP.Rel 0);
	     ppsay "local"; nl_indent ppstrm 2;
	     ppDec'(inner,d-1); newline ppstrm;
	     ppsay "in ";
	     ppDec'(outer,d-1); newline ppstrm;
	     ppsay "end";
	     closeBox ppstrm)
        | ppDec'(SeqDec decs,d) =
	    (openHVBox ppstrm (PP.Rel 0);
	     ppSequence ppstrm
	       {sep=newline,
	        pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	        style=CONSISTENT}
	        decs;
	     closeBox ppstrm)
        | ppDec'(OpenDec strbs,d) = 
	  (openHVBox ppstrm (PP.Rel 0);
	   ppsay "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => break ppstrm {nsp=1,offset=0}),
	      pr=(fn ppstrm => fn sp => pp_symbol_list sp),
	      style=INCONSISTENT}
             strbs;
	   closeBox ppstrm)
        | ppDec'(OvldDec (sym, ty, explist),d) = ppSym ppstrm sym
	| ppDec'(FixDec {fixity,ops},d) =
	  (openHVBox ppstrm (PP.Rel 0);
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

        | ppDec'(MarkDec(dec,(s,e)),d) =  
	   (case source_opt
	      of SOME source =>
	         (ppsay "MarkDec(";
		  ppDec'(dec,d); ppsay ",";
		  prpos(ppstrm,source,s); ppsay ",";
		  prpos(ppstrm,source,e); ppsay ")")
	       | NONE => ppDec'(dec,d))
	  
     in ppDec'
    end

and ppVb (context as (env,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
	fun ppVb'(_,0)= ppsay "<binding>"
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
    let val ppsay = PP.string ppstrm
	fun ppRvb'(_,0)= ppsay "<rec binding>"
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
    let val ppsay = PP.string ppstrm
	fun ppFb'(_,0)= ppsay "<FunBinding>"
	  | ppFb'(Fb (clauses, ops),d) =
              ppvlist ppstrm (head, "  | ",
	       (fn ppstrm => fn (cl: clause) => (ppClause context ppstrm (cl,d))),
               clauses)
	  | ppFb'(MarkFb (t,r),d) = ppFb context ppstrm head (t,d)
    in
	ppFb'
    end

and ppClause (context as (_,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
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
    let val ppsay = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar, d))
	     in ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm "*";
                                     break ppstrm {nsp=1,offset=0})),
		  pr=pr,
		  style=INCONSISTENT}
		 symbol_list
	    end

	  fun ppTb'(_,0)= ppsay "<T.binding>"
	    | ppTb'(Tb{tyc,def,tyvars},d) = 
		(openHOVBox ppstrm (PP.Rel 0);
	  	  ppSym ppstrm tyc; PP.string ppstrm " =";
	  	  break ppstrm {nsp=1,offset=0}; ppTy context ppstrm (def, d);
		  pp_tyvar_list (tyvars,d);
	  	 closeBox ppstrm)
	    | ppTb'(MarkTb (t,r),d) = ppTb context ppstrm (t,d)
    in
	ppTb'
    end

and ppDb (context as (_,source_opt)) ppstrm  = 
    let val ppsay = PP.string ppstrm
	fun pp_tyvar_list (symbol_list, d) =
	    let fun pr _ (tyvar) = (ppTyvar context ppstrm (tyvar, d))
	     in ppSequence ppstrm
		 {sep=(fn ppstrm => (PP.string ppstrm "*";
				     break ppstrm {nsp=1,offset=0})),
		  pr=pr,
		  style=INCONSISTENT}
		 symbol_list
	    end
	fun ppDb'(_,0)= ppsay "<D.binding>"
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
    let val ppsay = PP.string ppstrm
	fun ppDbrhs'(_,0)= ppsay "<dbrhs>"
	  | ppDbrhs'(Constrs const,d) = 
	    let fun pr ppstrm (sym:symbol, tv:Ast.ty option) =
		    (case tv
		       of SOME a =>
			  (ppSym ppstrm sym; ppsay" of "; ppTy context ppstrm (a, d))
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
    let val ppsay = PP.string ppstrm
        val pp_symbol_list = pp_path ppstrm
	fun ppEb'(_,0)= ppsay "<Eb>"
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
    let val ppsay = PP.string ppstrm
	fun ppStrb'(_,0)= ppsay "<Strb>"
	  | ppStrb'(Strb{name,def,constraint},d) = 
	     (openHVBox ppstrm (PP.Rel 0);
	      ppSym ppstrm name; PP.string ppstrm " :";
	      (* ???? missing constraint *)
	      break ppstrm {nsp=1,offset=2}; ppStrExp context ppstrm (def,d-1);
	      closeBox ppstrm)
	  | ppStrb'(MarkStrb (t,r),d) = ppStrb context ppstrm (t,d)
    in
	ppStrb'
    end

and ppFctb (context as (_,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
	fun ppFctb'(_,0)= ppsay "<Fctb>"
	  | ppFctb'(Fctb{name,def=BaseFct{params,body,constraint}},d) =
	    (openHVBox ppstrm (PP.Rel 0);
             ppSym ppstrm name;
             let fun pr ppstrm (SOME symbol, sigexp) =
                     (ppsay "("; ppSym ppstrm symbol; ppsay " : ";
                      ppSigExp context ppstrm (sigexp, d); ppsay ")")
                   | pr ppstrm (NONE, sigexp) =
                     (ppsay "("; ppSigExp context ppstrm (sigexp, d); ppsay ")")
              in (ppSequence ppstrm
                    {sep=(fn ppstrm => (break ppstrm {nsp=1,offset=0})),
                     pr = pr,
                     style = INCONSISTENT}
                    params;
                  case constraint
                    of NoSig => ()
                     | Transparent(sigexp) => 
                       (ppsay " :"; break ppstrm {nsp=1,offset=2};
                        ppSigExp context ppstrm (sigexp,d))
                     | Opaque(sigexp) => 
                       (ppsay " :>"; break ppstrm {nsp=1,offset=2};
                        ppSigExp context ppstrm (sigexp,d));
                  nbSpace ppstrm 1;
                  ppsay "="; break ppstrm {nsp=1,offset=0};
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
    let val ppsay = PP.string ppstrm
	fun ppFsigb'(_,0)= ppsay "<Fsigb>"
	  | ppFsigb'(Fsigb{name,def},d) = 
	    (openHVBox ppstrm (PP.Rel 0);
	      ppsay "funsig "; ppSym ppstrm name; ppsay " =";
	      break ppstrm {nsp=1,offset=2}; ppFsigExp context ppstrm (def,d-1);
	     closeBox ppstrm)
	  | ppFsigb'(MarkFsigb (t,r),d) = ppFsigb context ppstrm (t,d)
    in
	ppFsigb'
    end

and ppTyvar (context as (_,source_opt)) ppstrm =
    let val ppsay = PP.string ppstrm
	fun ppTyvar'(_,0)= ppsay "<tyvar>"
	  | ppTyvar'(Tyv s,d) = (ppSym ppstrm s) 
	  | ppTyvar'(MarkTyv (t,r),d) = ppTyvar context ppstrm (t,d)
    in
	ppTyvar'
    end

and ppTy (context as (env, source_opt)) ppstrm =			
  let val ppsay = PP.string ppstrm
      fun ppTy' (_,0) = ppsay "<type>"
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
                         (ppTy' (dom,d-1); ppsay " ->"; break ppstrm {nsp=1,offset=2};
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
		  (ppSym ppstrm sym; ppsay ":"; ppTy context ppstrm (tv, d))
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
                 ppsay "("; 
                 ppTy' (ty,d); 
                 ppsay ")";
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

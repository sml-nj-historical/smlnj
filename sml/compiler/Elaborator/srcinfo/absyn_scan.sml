structure Ens_absyn = 
struct
    local 
	open Absyn
	structure EV = Ens_var2
    in
    
        val ref_str = (ref [] : {def:strexp, name:Symbol.symbol, str:Modules.Structure} list ref)
	fun push str = ref_str := str :: !ref_str
	fun pop () = 
	    case !ref_str of
		[] => ErrorMsg.impossible "Ens_absyn.pop"
	      | _::q => ref_str := q
	fun head () = 
	    case !ref_str of
		[] => ErrorMsg.impossible "Ens_absyn.head"
	      | h::_ => h
	fun str_par () = 
	    case !ref_str of
		[] => ErrorMsg.impossible "Ens_absyn.str_par"
	      | [_] => NONE
	      | _::{str=Modules.STR{access, ...}, ...}::_ => SOME access
	      | _ => ErrorMsg.impossible "Ens_absyn.str_par2"

        fun scan_exp exp = 
	    case exp of 
		VARexp _ => ()
	      | CONexp _ => ()
	      | INTexp _ => ()
	      | WORDexp _ => ()
	      | REALexp _ => ()
	      | STRINGexp _ => ()
	      | CHARexp _ => ()
	      | RECORDexp list => List.app (scan_exp o #2) list
	      | SELECTexp (_, exp) => scan_exp exp
	      | VECTORexp (expl, _) => List.app scan_exp expl
	      | PACKexp (exp, _, _) => scan_exp exp
	      | APPexp (e1, e2) => (scan_exp e1;scan_exp e2)
	      | HANDLEexp (exp, fnrules) => 
		( scan_exp exp;
		  List.app scan_rule (#1 fnrules)
		)
	      | RAISEexp (exp, _) => scan_exp exp
	      | CASEexp (exp, rulel, _) => 
		( scan_exp exp; 
		  List.app scan_rule rulel
		)
	      | IFexp {test = e1, thenCase = e2, elseCase = e3} => 
		(scan_exp e1; scan_exp e2; scan_exp e3)
	      | ANDALSOexp (e1, e2) => (scan_exp e1;scan_exp e2)
	      | ORELSEexp (e1, e2) => (scan_exp e1;scan_exp e2)
	      | WHILEexp {test = e1, expr = e2} => (scan_exp e1;scan_exp e2)
	      | FNexp fnrules => List.app scan_rule (#1 fnrules)
	      | LETexp (dec, exp) => (scan_dec dec; scan_exp exp)
	      | SEQexp expl => List.app scan_exp expl
	      | CONSTRAINTexp (exp, _) => scan_exp exp
	      | MARKexp (exp, region) => 
		( case exp of 
		      VARexp (ref var, tyvarl) => 
		      EV.add_var_use var region tyvarl
		    | _ => ();
		  scan_exp exp
		)
		
	and scan_rule rule = 
	    case rule of
		RULE (pat, exp) => (
		case pat of 
		    VARpat var => EV.add_var_def var (~1, ~1) (head ())
		  | _ => ();
		scan_pat pat; scan_exp exp)
			  
	and scan_pat pat = 
	    case pat of
		WILDpat => ()
	      | VARpat var => ()
	      | INTpat _ => ()
	      | WORDpat _ => ()
	      | REALpat _ => ()
	      | STRINGpat _ => ()
	      | CHARpat _ => ()
	      | CONpat _ => ()
	      | RECORDpat {fields, ...} => List.app (scan_pat o #2) fields
	      | APPpat (_, _, pat) => scan_pat pat
	      | CONSTRAINTpat (pat, _) => scan_pat pat
	      | LAYEREDpat (p1, p2) => (scan_pat p1; scan_pat p2)
	      | ORpat (p1, p2) => (scan_pat p1; scan_pat p2)
	      | VECTORpat (patl, _) => List.app scan_pat patl
	      | NOpat => ()

	and scan_dec dec =
	    case dec of
		VALdec vbl => List.app scan_vb vbl
	      | VALRECdec rvbl => List.app scan_rvb rvbl
	      | TYPEdec _ => ()
	      | DATATYPEdec _ => ()
	      | ABSTYPEdec {body, ...} => scan_dec body
	      | EXCEPTIONdec ebl => List.app scan_eb ebl
	      | STRdec strbl => List.app scan_strb strbl
	      | ABSdec strbl => List.app scan_strb strbl
	      | FCTdec fctbl => List.app scan_fctb fctbl
	      | SIGdec _ => ()
	      | FSIGdec _ => ()
	      | OPENdec _ => ()
	      | LOCALdec (d1, d2) => (scan_dec d1; scan_dec d2)
	      | SEQdec decl => List.app scan_dec decl
	      | OVLDdec _ => ()
	      | FIXdec _ => ()
	      | MARKdec (dec, region) => 
		( case dec of
		      VALdec [VB {pat = VARpat var, ...}] => 
		      EV.add_var_def var region (head ())
		    | _ => ();
		  scan_dec dec
		)
			     
	and scan_strexp strexp = 
	    case strexp of
		VARstr s => (
		case s of
		    Modules.STR {access, ...} =>
		    let fun is_ext (Access.EXTERN _) = true
			  | is_ext (Access.PATH (s, _)) = is_ext s
			  | is_ext _ = false
		    in
			if is_ext access then
			    ()
			else (
			    EV.add_str_alias (head ()) s (~1, ~1) (str_par ());
			    EV.add_str_use s (~1, ~1)
			    )
		    end
		  | _ => ()
		)
	      | STRstr bl => 
		EV.add_str_def (head ()) bl (~1, ~1) (str_par ())
	      | APPstr _ => pop ()
	      | LETstr (dec, strexp) => 
		( scan_dec dec; 
		  scan_strexp strexp)
	      | MARKstr (strexp, _) => scan_strexp strexp

	and scan_fctexp fctexp =
	    case fctexp of
		VARfct _ => ()
	      | FCTfct {def, ...} => scan_strexp def
	      | LETfct (dec, fctexp) => (scan_dec dec; scan_fctexp fctexp)
	      | MARKfct (fctexp, _) => scan_fctexp fctexp

	and scan_vb vb = 
	    case vb of
		VB {pat, exp, ...} => (scan_pat pat; scan_exp exp)

				 
	and scan_rvb rvb = 
	    case rvb of
		RVB {exp, ...} => scan_exp exp

	and scan_eb eb = 
	    case eb of 
		EBgen {ident, ...} => scan_exp ident
	      | EBdef _ => ()

	and scan_strb strb = 
	    case strb of
		STRB (r as {def, name, str}) => 
		(
		 push r;
		 scan_strexp def ;
		 pop ()
		)

	and scan_fctb fctb = 
	    case fctb of
		FCTB {def, ...} => scan_fctexp def
    end
end

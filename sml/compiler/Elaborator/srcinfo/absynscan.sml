structure AbsynScan = 
struct
    local 
	open Absyn
	structure DB = Database
    in
        val ref_str = 
	    (ref [] : {def:strexp, 
		       name:Symbol.symbol, 
		       str:Modules.Structure} list ref)
	    
	fun bug x = ErrorMsg.impossible ("AbsynScan: " ^ x)

	fun push str = ref_str := str :: !ref_str
	fun pop () = 
	    case !ref_str of
		[] => bug "pop"
	      | _::q => ref_str := q
	fun head () = 
	    case !ref_str of
		[] => bug "head"
	      | h::_ => h
	fun str_par () = 
	    case !ref_str of
		[] => bug "str_par"
	      | [_] => NONE
	      | _::{str=Modules.STR{access, ...}, ...}::_ => SOME access
	      | _ => bug "str_par2"


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
	      | CONSTRAINTexp (exp, ty) => 
		(DB.add_ty_use ty (~1, ~1); scan_exp exp)
	      | MARKexp (exp, region) => 
		( case exp of 
		      VARexp (ref var, tyvarl) => 
		      DB.add_var_use var region tyvarl
		    | _ => ();
		  scan_exp exp
		)
		
	and scan_rule rule = 
	    case rule of
		RULE (pat, exp) => (
		case pat of 
		    VARpat var => DB.add_var_def var (~1, ~1) (head ())
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

	and scan_markedtycon (MARKtyc (tycon, region)) = 
	    DB.add_ty_def tycon region

	and scan_dec dec =
	    case dec of
		VALdec vbl => List.app scan_vb vbl
	      | VALRECdec rvbl => List.app scan_rvb rvbl
	      | TYPEdec tl => List.app scan_markedtycon tl
	      | DATATYPEdec {datatycs, withtycs} => 
		( List.app scan_markedtycon withtycs;
		  List.app scan_markedtycon datatycs
		)
	      | ABSTYPEdec {body, abstycs, withtycs} => 
		( List.app scan_markedtycon withtycs;
		  List.app scan_markedtycon abstycs;
		  scan_dec body
		)
	      | EXCEPTIONdec ebl => List.app scan_eb ebl
	      | STRdec strbl => List.app scan_strb strbl
	      | ABSdec strbl => List.app scan_strb strbl
	      | FCTdec fctbl => List.app scan_fctb fctbl
	      | SIGdec sigl => 
		List.app (fn x => DB.add_sig_def x (~1, ~1)) sigl
	      | FSIGdec _ => ()
	      | OPENdec _ => ()
	      | LOCALdec (d1, d2) => (scan_dec d1; scan_dec d2)
	      | SEQdec decl => List.app scan_dec decl
	      | OVLDdec _ => ()
	      | FIXdec _ => ()
	      | MARKdec (dec, region) => 
		( case dec of
		      VALdec [VB {pat = VARpat var, ...}] => 
		      DB.add_var_def var region (head ())
		    | _ => ();
		  scan_dec dec
		)
			     
	and scan_strexp strexp =
	    let fun add s region = 
		    ( DB.add_str_alias 
			  (head ()) 
			  s 
			  region
			  region (* pas defaut *)
			  (str_par ());
		      DB.add_str_use 
			  s 
			  region
		    )
	    in
		case strexp of
		    VARstr s => add s (~12, ~12)
		  | STRstr bl => 
		    DB.add_str_def (head ()) NONE bl (~5, ~5) (str_par ())
		  | APPstr _ => pop ()
		  | LETstr (dec, strexp) => 
		    (scan_dec dec; scan_strexp strexp)
		  | MARKstr (strexp, region) => 
		    ( case strexp of
			  STRstr bl => 
			  DB.add_str_def (head ()) NONE bl region (str_par ())
			| VARstr s => 
			  add s region
			| _ => scan_strexp strexp
		    )
	    end
				 
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

	and scan_strb (STRB (r as {def, name, str})) =
	    (
	     push r;
	     case def of
		 LETstr (MARKdec (SEQdec [],rRHS), MARKstr (VARstr rhs, rID))=>
		 (* structure s2 = s *)
		 DB.add_str_alias r rhs rID rRHS (str_par ())
	       | LETstr
		     (SEQdec [MARKdec (SEQdec [], rRHS), dec],
		      MARKstr (VARstr s, rID)) =>
		 ( case recognize_strdec dec of
		       SOME (dec1, bl1) => 
		       (* structure s2 : sig = s*)
		       ( scan_dec dec1; 
			 DB.add_str_sig_alias r s bl1 rID rRHS (str_par ())
		       )
		     | NONE => (scan_dec dec)
		 )
	       | LETstr
		     (SEQdec [dec1, dec2],
		      MARKstr (VARstr s, region)) =>
		 ( case (recognize_strdec dec1, recognize_strdec dec2) of
		       (SOME (dec1, bl1), SOME (dec2, bl2)) =>
		       (* structure s : sig = struct end*)
		       ( scan_dec dec1;
			 (*scan_dec dec2; SHOULD BE CHECKED *)
			 DB.add_str_def r (SOME bl1) bl2 region (str_par ())
		       )
		     | _ => (scan_dec dec1; scan_dec dec2)
		 )
	       | LETstr
		     (dec, MARKstr (VARstr s, region)) =>
		 ( case recognize_strdec dec of
		       (* structure s = struct end *)
		       SOME (dec1, bl1) => 
		       ( scan_dec dec1;
			 DB.add_str_def r NONE bl1 region (str_par ())
		       )
		     | NONE => scan_dec dec
		 )
	       | _ => scan_strexp def;
	     pop ()
	    )

	and recognize_strdec dec = 
	    case dec of
		STRdec [STRB {def = LETstr (dec1, MARKstr (STRstr bl, _)),...}]
		=> SOME (dec1, bl)
	      | _ => NONE

	and scan_fctb fctb = 
	    case fctb of
		FCTB {def, ...} => scan_fctexp def
    end
end

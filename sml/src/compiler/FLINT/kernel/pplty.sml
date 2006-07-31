(* pplty.sml
 * 
 * (c) 2006 SML/NJ Fellowship
 *
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

structure PPLTy =
struct

fun ppList {sep, pp} list =
    (ppSequence ppstrm
	        {sep = fn ppstrm => (PP.break ppstrm {nsp=1, offset=0};
				     PP.string ppstrm sep),
		 style = INCONSISTENT,
		 pr = (fn _ => fn elem => 
				   (openHOVBox 1;
				    pps "(";
				    pp elem;
				    pps ")";
				    closeBox()))}
		list)

(* ppTKind : tkind -> unit 
 * Print a hashconsed representation of the kind *)
fun ppTKind (tk : TK.tkind) =
    let fun ppTKindI(LK.TK_MONO) = "TK_MONO"
	  | ppTKindI(LK.TK_BOX) = "TK_BOX"
	  | ppTKindI(LK.TK_FUN (argTkinds, resTkind)) = 
	      (* res_tkind is a TK_SEQ wrapping some tkinds 
	       * These are produced by Elaborate/modules/instantiate.sml 
	       *)
	     (openHOVBox 1;
	      pps "TK_FUN (";
	      ppList {sep="* ", pp=ppTKindI} argTkinds;
	      ppTKind resTkind;
	      pps ")";
	      closeBox())
	  | ppTKindI(LK.TK_SEQ tkinds) =
	    (openHOVBox 1;
	     pps "TK_SEQ(";
	     ppList {sep=", ", pp=ppTKindI} tkinds;
	     pps ")";
	     closeBox())
    in ppTKindI (LK.tk_out tk)
    end
	    
fun ppTyc (tycon : tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    let fun ppTycI (LK.TC_VAR(depth, cnt)) =
	    (pps "TC_VAR(";
	     (* depth is a deBruijn index set in elabmod.sml/instantiate.sml *)
	     pps (DebIndex.di_print depth);
	     pps ",";
	     (* cnt is computed in instantiate.sml sigToInst *)
	     pps (Int.toString cnt);
	     pps ")")
	  (* Named tyc VAR; is actually an lvar *)
	  | ppTycI (LK.TC_NVAR tvar) =
	    (pps "TC_NVAR(";
	     pps (Int.toString tvar);
	     pps ")")
	  | ppTycI (LK.TC_PRIM primtycon) =
	    (pps "TC_PRIM(";
	     pps (PT.pt_print primtycon);
	     pps ")")
	  | ppTycI (LK.TC_FN (argTkinds, resultTyc)) =
	    (openHOVBox 1;
	     pps "TC_FN(";
	     ppList {sep="* ", pp=ppTKind} argTKinds;
	     pps ",";
	     ppTyc resultTyc;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_APP(contyc, tys)) =
	    (openHOVBox 1;
	     pps "TC_APP(";
	     ppTyc contyc;
	     pps ",";
	     ppList {sep="* ", pp=ppTycI} tys;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_SEQ tycs) =
	    (openHOVBox 1;
	     pps "TC_SEQ(";
	     ppList {sep=", ", pp=ppTycI} tycs;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_PROJ(tycon, index)) =
	    (openHOVBox 1;
	     pps "TC_PROJ(";
	     ppTycI tycon;
	     pps ", ";
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_SUM(tycs)) =
	    (pps "TC_SUM(";
	     ppList {sep=", ", pp=ppTycI} tycs;
	     pps ")")
	    (* TC_FIX is a recursive DATATYPE *)
	  | ppTycI (LK.TC_FIX((numStamps, datatypeFamily, freetycs), index)) =
	    (openHOVBox 1;
	     pps "TC_FIX(";
	     pps "nStamps = ";
	     pps (Int.toString numStamps);
	     pps ", ";
	     pps "datatypeFamily = ";
	     ppTycI datatypeFamily;
	     pps ", ";
	     pps "freeTycs = ";
	     ppList {sep = ", ", pp = ppTycI} freetycs;
	     pps ", ";
	     pps "index = ";
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_ABS tyc) =
	    (pps "TC_ABS(";
	     ppTycI tyc;
	     pps ")")
	  | ppTycI (LK.TC_BOX tyc) =
	    (pps "TC_BOX(";
	     ppTycI tyc;
	     pps ")")
	    (* rflag is a tuple kind template, a singleton datatype RF_TMP *)
	  | ppTycI (LK.TC_TUPLE (rflag, tycs)) =
	    (pps "TC_TUPLE(";
	     ppList {sep="* ", pp=ppTycI} tycs;
	     pps ")")
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	  | ppTycI (TC_ARROW (fflag, argTycs, resTycs)) =
	    (pps "TC_ARROW(";
	     (case fflag of LK.FF_FIXED => pps "FF_FIXED"
			  | LK.FF_VAR(b1, b2) => (pps "FF_VAR(";
						  ppBool b1;
						  pps ", ";
						  ppBool b2;
						  pps ")"))
	     ppList {sep="* ", pp=ppTycI} argTycs;
	     pps ", ";
	     ppList {sep="* ", pp=ppTyci} resTycs;
	     pps ")")
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | ppTycI (TC_PARROW (argTyc, resTyc)) =
	    (pps "TC_PARROW(";
	     ppTycI argTyc;
	     pps ", ";
	     ppTycI resTyc;
	     pps ")")
	  | ppTycI (TC_TOKEN (tok, tyc)) =
	    (pps "TC_TOKEN(";
	     pps (Int.toString tok);
	     pps ", ";
	     ppTycI tyc;
	     pps ")")
	  | ppTycI (TC_CONT tycs) = 
	    (pps "TC_CONT(";
	     ppList {sep=", ", pp=ppTyc} tycs;
	     pps ")")
	  | ppTycI (TC_IND (tyc, tycI)) =
	    (openHOVBox 1;
	     pps "TC_IND(";
	     ppTyc tyc;
	     pp ", ";
	     ppTycI tycI;
	     pps ")";
	     closeBox())
	  | ppTycI (TC_ENV (tyc, ol, nl, tenv)) =
	    (openHOVBox 1;
	     pps "TC_ENV(";
	     ppTyc tyc;
	     pps ", ";
	     pps "ol = ";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl = ";
	     pps (Int.toString nl);
	     pps ", ";
	     ppTyc tenv;
	     closeBox())
    in ppTycI (LK.tc_out tycon)
    end
	    
	     
end

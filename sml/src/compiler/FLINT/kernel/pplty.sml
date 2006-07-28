(* pplty.sml 
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

structure PPLTy =
struct

(* ppTKind : tkind -> unit 
 * Print a hashconsed representation of the kind *)
fun ppTKind (tk : TK.tkind) =
    let fun ppTKindI(LK.TK_MONO) = "TK_MONO"
	  | ppTKindI(LK.TK_BOX) = "TK_BOX"
	  | ppTKindI(LK.TK_FUN (arg_tkinds, res_tkind)) = 
	     (openHOVBox 1;
	      pps "TK_FUN (";
	      (ppSequence ppstrm
	        {sep = fn ppstrm => (PP.break ppstrm {nsp=1, offset=0};
				     PP.string ppstrm "* "),
		 style = INCONSISTENT,
		 pr = (fn _ => fn tk => 
				   (openHOVBox 1;
				    pps "(";
				    (* Print Kind *)
				    pps ")";
				    closeBox()))}
		arg_tkinds);
	     ppTKind res_tkind;
	     pps ")")
	  | ppTKindI(LK.TK_SEQ tkinds) =
	    (openHOVBox 1;
	     pps "TK_SEQ ";
	     (ppSequence ppstrm
	       {sep = fn ppstrm => (PP.break ppstrm {nsp=1, offset=0};
				    PP.string ppstrm ", "),
		style = INCONSISTENT,
		pr = (fn _ => fn tk =>
				 (openHOVBox 1;
				  pps "(";
				  (* Print Kind *)
				  pps ")";
				  closeBox()))}
	       tkinds))
    in ppTKindI (LK.tk_out tk)
    end
	    
fun ppTyc (tycon : tyc) =
    let fun ppTycI (LK.TC_VAR())
end

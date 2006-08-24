(* pplty.sml
 * 
 * (c) 2006 SML/NJ Fellowship
 *
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

signature PPLTY =
sig

end

structure PPLty =
struct

local 

    structure PT = PrimTyc
    structure PP = PrettyPrintNew
    open PPUtilNew
in

val dtPrintNames : bool ref = ref true

fun ppSeq ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
     in	ppSequence ppstrm
	   {sep = fn ppstrm => (PP.string ppstrm sep;
			        PP.break ppstrm {nsp=1, offset=0}),
	    style = INCONSISTENT,
            pr = pp}
           list
    end (* ppSeq *)

fun ppList ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    ppClosedSequence ppstrm
      {front = fn ppstrm => (PP.string ppstrm "["),
       back = fn ppstrm => (PP.string ppstrm "]"),
       sep = fn ppstrm => (PP.string ppstrm sep;
		           PP.break ppstrm {nsp=1, offset=0}),
       style = INCONSISTENT,
       pr = pp}
      list

(* ppTKind : tkind -> unit 
 * Print a hashconsed representation of the kind *)
fun ppTKind pd ppstrm (tk : Lty.tkind) =
    if pd < 1 then pps ppstrm "<tk>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
        val ppTKind' = ppTKind (pd-1) ppstrm
	val ppList' = ppList ppstrm
	fun ppTKindI(Lty.TK_MONO) = pps "MK"
	  | ppTKindI(Lty.TK_BOX) = pps "BK"
	  | ppTKindI(Lty.TK_FUN (argTkinds, resTkind)) = 
	      (* res_tkind is a TK_SEQ wrapping some tkinds 
	       * These are produced by Elaborate/modules/instantiate.sml 
	       *)
	     (openHOVBox 1;
	       pps "(";
	       ppList' {sep=",", pp=ppTKind (pd-1)} argTkinds;
	       pps "=>"; ppTKind' resTkind;
	       pps ")";
	      closeBox())
	  | ppTKindI(Lty.TK_SEQ tkinds) =
	     (openHOVBox 1;
	       pps "SK";
	       ppList' {sep=",", pp=ppTKind (pd-1)} tkinds;
	      closeBox())
     in ppTKindI (Lty.tk_outX tk)
    end (* ppTKind *)

fun tycEnvFlatten(tycenv) = 
    (case Lty.teDest(tycenv)
       of NONE => []
        | SOME(elem, rest) => elem::tycEnvFlatten(rest))

fun ppKeFrame pd ppstrm ks =
    ppList ppstrm {sep=",", pp=ppTKind pd} ks

fun ppKindEnv pd ppstrm kenv =
    if pd < 1 then pps ppstrm "<tkenv>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
     in pps "[";
        openHOVBox 1;
        ppList ppstrm {sep=",",pp=ppKeFrame (pd-1)} kenv;
        closeBox ();
        pps "]"
    end

fun ppTEBinder pd ppstrm (binder: Lty.teBinder) =
    if pd < 1 then pps ppstrm "<teBinder>" else
    let val {openHOVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
    in openHOVBox 1;
	let val ks = 
		(case binder
		  of Lty.Lamb (level, ks) => (pps "L"; ppi level; ks) 
		   | Lty.Beta (level, args, ks) =>
	             (pps "B";
		      ppi level;
		      pps "(";
		      ppList ppstrm {sep=",", pp=ppTyc (pd-1)} args;
		      pps ")";
		      ks)
		)
	in if pd < 2 then () 
	   else (pps " : ";
                 ppKeFrame (pd-1) ppstrm ks)
	end;
       closeBox()
    end (* function ppTEBinder *)

and ppTyc pd ppstrm (tycon : Lty.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
	val ppList' : {pp:PP.stream -> 'a -> unit, sep: string} -> 'a list -> unit =
              fn x => ppList ppstrm x
	       (* eta-expansion of ppList to avoid value restriction *) 

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppTyc' = ppTyc (pd-1) ppstrm

	fun ppTycI (Lty.TC_VAR(depth, cnt)) =
	    (pps "TV(";
	     (* depth is a deBruijn index set in elabmod.sml/instantiate.sml *)
	     pps (DebIndex.di_print depth);
	     pps ",";
	     (* cnt is computed in instantiate.sml sigToInst or 
	        alternatively may be simply the IBOUND index *)
	     ppi cnt;
	     pps ")")
	  (* Named tyc VAR; is actually an lvar *)
	  | ppTycI (Lty.TC_NVAR tvar) =
	    (pps "NTV:"; ppi tvar)
	  | ppTycI (Lty.TC_PRIM primtycon) =
	    (pps "PRIM(";
	     pps (PT.pt_print primtycon);
	     pps ")")
	  | ppTycI (Lty.TC_FN (argTkinds, resultTyc)) =
	    (openHOVBox 1;
	     pps "FN(";
	     ppList' {sep="*", pp=ppTKind (pd-1)} argTkinds;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' resultTyc;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_APP(contyc, tys)) =
	    (openHOVBox 1;
	     pps "APP(";
	     ppTyc' contyc;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} tys;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_SEQ tycs) =
	    (openHOVBox 1;
	     pps "SEQ(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_PROJ(tycon, index)) =
	    (openHOVBox 1;
	     pps "PROJ(";
	     ppTyc' tycon;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_SUM(tycs)) =
	    (pps "SUM(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	    (* TC_FIX is a recursive datatype constructor 
	       from a (mutually-)recursive family *)
	  | ppTycI (Lty.TC_FIX{family={size,names,gen,params},index}) =
            if !dtPrintNames then pps (Vector.sub(names,index))
            else
	    (openHOVBox 1;
              pps "FIX(";
              openHVBox 0;
              pps "size = "; ppi size; PP.break ppstrm {nsp=1,offset=0};
              pps "index = "; ppi index; PP.break ppstrm {nsp=1,offset=0};
              pps "gen = ";
              openHOVBox 2;
               ppTyc' gen;
              closeBox;
              pps "prms = ";
              openHOVBox 2;
               ppList' {sep = ",", pp = ppTyc (pd-1)} params;
              closeBox ();
              PP.break ppstrm {nsp=0,offset=0};
              pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_ABS tyc) =
	    (pps "ABS(";
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (Lty.TC_BOX tyc) =
	    (pps "BOX(";
	     ppTyc' tyc;
	     pps ")")
	    (* rflag is a tuple kind template, a singleton datatype RF_TMP *)
	  | ppTycI (Lty.TC_TUPLE(rflag, tycs)) =
	    (ppClosedSequence ppstrm
                {front = (fn s => PP.string s "{"),
                 sep =  (fn s => PP.string s ","),
                 back =  (fn s => PP.string s "}"),
                 pr = ppTyc (pd-1),
                 style = INCONSISTENT}
	        tycs)
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	  | ppTycI (Lty.TC_ARROW (fflag, argTycs, resTycs)) =
	    (pps "ARR(";
	     (case fflag of Lty.FF_FIXED => pps "FF_FIXED"
			  | Lty.FF_VAR(b1, b2) =>
                              (pps "<FF_VAR>" (*; ppBool b1; pps ",";
						  ppBool b2; pps ")"*) ));
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} argTycs;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} resTycs;
	     pps ")")
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | ppTycI (Lty.TC_PARROW (argTyc, resTyc)) =
	    (pps "PARR(";
	     ppTyc' argTyc;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' resTyc;
	     pps ")")
	  | ppTycI (Lty.TC_TOKEN (tok, tyc)) =
	    (pps "TOK(";
	     pps (Lty.token_name tok);
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (Lty.TC_CONT tycs) = 
	    (pps "CONT(";
	     ppList' {sep=", ", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	  | ppTycI (Lty.TC_IND (tyc, tycI)) =
	    (openHOVBox 1;
	     pps "IND(";
	     ppTyc' tyc;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTycI tycI;
	     pps ")";
	     closeBox())
	  | ppTycI (Lty.TC_ENV (tyc, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
	     closeBox())
    in ppTycI (Lty.tc_outX tycon)
    end (* ppTyc *)


fun ppTycEnv pd ppstrm (tycEnv : Lty.tycEnv) =
    if pd < 1 then pps ppstrm "<tycEnv>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
     in openHOVBox 1;
	 pps "TycEnv(";
	 ppList ppstrm {sep=", ", pp=ppTEBinder (pd-1)} (tycEnvFlatten tycEnv);
	 pps ")";
	closeBox()
    end (* ppTycEnv *)

	     
fun ppLty pd ppstrm (lty: Lty.lty) =
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
	val ppList' : {pp:PP.stream -> 'a -> unit, sep: string} -> 'a list -> unit =
              fn x => ppList ppstrm x
	       (* eta-expansion of ppList to avoid value restriction *) 

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppLty' = ppLty (pd-1) ppstrm

        fun ppLtyI (Lty.LT_TYC tc) =
            (pps "TYC("; ppTyc pd ppstrm tc; pps "0")
          | ppLtyI (Lty.LT_STR ltys) =
            (pps "STR("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (Lty.LT_FCT (args,res)) =
            (pps "FCT("; ppList' {sep=",",pp=ppLty (pd-1)} args; pps ",";
             PP.break ppstrm {nsp=1,offset=0};
             ppList' {sep=",",pp=ppLty (pd-1)} res; pps ")")
          | ppLtyI (Lty.LT_POLY (ks,ltys)) =
	    (openHOVBox 1;
	     pps "FN(";
	     ppList' {sep="*", pp=ppTKind (pd-1)} ks;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",",pp=ppLty (pd-1)} ltys;
	     pps ")";
	     closeBox())
          | ppLtyI (Lty.LT_CONT ltys) =
            (pps "CONT("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (Lty.LT_IND(nt,ot)) =
            (pps "IND("; ppLty (pd-1) ppstrm nt; pps ",";
             PP.break ppstrm {nsp=1,offset=0};
             ppLtyI ot; pps ")")
	  | ppLtyI (Lty.LT_ENV (lty, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "LT_ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLty' lty;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
	     closeBox())
    in ppLtyI (Lty.lt_outX lty)
    end (* ppLty *)

end (* local *)	    

end (* structure PPLty *)

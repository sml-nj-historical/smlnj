(* absynutil.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * More stuff from ElabUtil should be moved here eventually.
 *)
structure AbsynUtil :
  sig

    val unitExp : Absyn.exp

    val TUPLEexp : Absyn.exp list -> Absyn.exp
    val TUPLEpat : Absyn.pat list -> Absyn.pat

    val delcaredSymbols: Absyn.dec -> Symbol.symbol list

  end =

struct

  structure A = Absyn

  fun bug msg = ErrorMsg.impossible("AbsynUtil: "^msg)

  val unitExp = A.RECORDexp []

  fun TUPLEexp l = let
      fun build (_, []) = []
	| build (i, e :: es) =
	  (A.LABEL { number = i-1, name = Tuples.numlabel i }, e)
	  :: build (i+1, es)
  in
      A.RECORDexp (build (1, l))
  end

  fun TUPLEpat l = let
      fun build (_, []) = []
	| build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
  in
      A.RECORDpat { fields = build (1, l), flex = false,
		    typ = ref Types.UNDEFty }
  end

  (* declaredSymbols : Absyn.dec -> symbol list 
   * returns the names declared in decl, in the order declared.
   * Used in extractSig in ElabMod *)
  fun declaredSymbols dec =
      let fun strbs([]) = []
	    | strbs((A.STRB{name,...})::rest) = name::(strbs rest)
	  fun pat(A.VARpat(V.VALvar{path,...})) = [SymPath.first path] 
	    | pat(A.VARpat(_)) = 
		bug "declaredNames -- Bad VARpat"
	    | pat(A.RECORDpat{fields,...}) = 
		foldl (fn ((_,p), names) => (pat p)@names) [] 
		      fields
	    | pat(A.APPpat(_,_,p)) = pat p
	    | pat(A.CONSTRAINTpat(p,_)) = pat p
	    | pat(A.LAYEREDpat(p,p')) = 
		(pat p)@(pat p')
	    | pat(A.ORpat(p,p')) = (pat p)@(pat p')
	    | pat(A.VECTORpat(ps,_)) = 
		foldl (fn (p,names) => (pat p)@names) [] ps
	    | pat _ = []
	  fun vbs([]) = []
	    | vbs((A.VB{pat=p,...})::rest) = 
		(pat p)@(vbs rest)
	  fun tycs([]) = []
	    | tycs(tyc::rest) = (TU.tycName tyc)::(tycs rest) 
	  fun datatycs([]) = []
	    | datatycs(T.GENtyc{kind=T.DATATYPE dt, path, ...}::rest) =
		let val {index,family as {members,...},...} = dt
		    val {tycname,dcons,...} = Vector.sub(members,index)
		    val pathname = InvPath.last path
		in (map (fn ({name,...}) => name) dcons)@
		   (pathname::datatycs rest)
		end 
	    | datatycs(_) = bug "declaredSymbols -- bad datatycs"
	  fun ebs([]) = []
	    | ebs((A.EBgen{exn=T.DATACON{name,...},...})::rest) = 
		name::(ebs rest)
	    | ebs((A.EBdef{exn=T.DATACON{name,...},...})::rest) =
		name::(ebs rest)
	  fun fctbs([]) = []
	    | fctbs(A.FCTB{name,...}::rest) = name::(fctbs rest)
	  fun str(M.STR{sign=M.SIG{elements,...},...}) = 
		MU.getElementsSymbols elements
	    | str(M.STR{sign=M.ERRORsig,...}) = []
	    | str(M.STRSIG{sign=M.SIG{elements,...},...}) =
		MU.getElementsSymbols elements
	    | str(M.STRSIG{sign=M.ERRORsig,...}) = 
		bug "declaredSymbols -- ERRORsig in STRSIG"
	    | str(M.ERRORstr) = []
	  fun rvbs([]) = []
	    | rvbs(A.RVB{var=V.VALvar{path,...},...}::rest) =
		(SymPath.first path)::(rvbs rest)
	    | rvbs(_::rest) = bug "delcaredNames -- Bad RVB"
      in case decl 
	  of A.STRdec(strbs') => strbs strbs'
	   | A.VALdec(vbs') => vbs vbs'
	   | A.VALRECdec(rvbs') => rvbs rvbs'
	   | A.TYPEdec(tycs') => tycs tycs'
	   | A.DATATYPEdec{datatycs,withtycs} => 
	       (rev (tycs withtycs))@(rev (datatycs datatycs))
	   | A.ABSTYPEdec{abstycs,withtycs,body} =>
	       (tycs abstycs)@(tycs withtycs)@(declaredSymbols body)
	   | A.EXCEPTIONdec(ebs') => ebs ebs'
	   | A.FCTdec(fctbs') => fctbs fctbs'
	   | A.OPENdec(pathstrs) => 
	       foldl (fn (str',names) => (rev (str str'))@names) [] 
		     (map #2 pathstrs)
	   | A.LOCALdec(_,dec) => (declaredSymbols dec) 
	   | A.SEQdec(decs) => 
	       foldl (fn (dec,names) => (declaredSymbols dec)@names) [] decs
	   | A.MARKdec(dec,_) => declaredSymbols dec
	   | A.FIXdec{ops,...} => []
	   | _ => bug "declaredNames - Unexpected dec"  
      end

end (* structure AbsynUtil *)

signature FLINT2LAMBDA =
sig
    type lvar = LambdaVar.lvar
    type lty = LtyDef.lty

    val transVal  : FLINT.value -> Lambda.value
    val transLexp : FLINT.lexp -> Lambda.lexp
    val transFundec : FLINT.fundec -> Lambda.lexp
end

structure Flint2Lambda :> FLINT2LAMBDA =
struct
structure F = FLINT
structure L = Lambda

structure A = Access
structure LV = LambdaVar
structure PO = PrimOp
structure S = Symbol
structure LT = LtyExtern

type lvar = LambdaVar.lvar
type lty = LtyDef.lty

fun bug msg = ErrorMsg.impossible("flint2lambda: "^msg)

(* tuple up a list of ltys into a single lty *)
fun ltTuple [lty] = lty
  | ltTuple ltys = LT.ltc_tuple ltys

fun transVal fval = 
    case fval of 
	F.VAR lvar => L.VAR lvar
      | F.INT i    => L.INT i
      | F.INT32 i  => L.INT32 i
      | F.WORD w   => L.WORD w
      | F.WORD32 w => L.WORD32 w
      | F.REAL s   => L.REAL s
      | F.STRING s => L.STRING s

fun id (value : L.value) (x : L.lexp) = x

fun transCon fcon =
    case fcon of
        F.INTcon i    => L.INTcon i
      | F.INT32con i  => L.INT32con i
      | F.WORDcon w   => L.WORDcon w
      | F.WORD32con w => L.WORD32con w
      | F.REALcon s   => L.REALcon s
      | F.STRINGcon s => L.STRINGcon s
      | F.VLENcon i   => L.VLENcon i
      | F.DATAcon (dcon,_,_)  => L.DATAcon dcon
	    
fun selectLoop record body =
    let 
	fun loop (_, []) = transLexp body
	  | loop (i, lvar::lvars) =
	    L.LET (lvar,
		   L.SELECT (i, L.VAR record),
		   loop (i+1, lvars))
    in
	loop
    end

and transFundec (fkind, lvar, (* lty, *) formals, lexpBody) =
    (case formals of
	 [(lvarArg, ltyArg)] =>		(* single arg function *)
	     L.FN (lvarArg, ltyArg, transLexp lexpBody)
       | _ => 
	     let
		 val (lvarArgs, ltyArgs) = ListPair.unzip formals
		 val lvarArg = LV.mkLvar()
		 val ltyArg = ltTuple ltyArgs
	     in
		 L.FN (lvarArg, ltyArg,
		       selectLoop lvarArg lexpBody (0, lvarArgs))
	     end)

(**
 ** Warning: I think we need to wrap the result as well !!! (ZHONG)
 **)    
and transFundecRec (fk as {isrec=NONE, raw, isfct}, _, _, _) = 
      bug "unexpected case in transFundecRec"
  | transFundecRec (fk as {isrec=SOME zs, raw, isfct}, 
                    lvar, formals, lexpBody) =
    (case formals of
	 [(lvarArg, ltyArg)] =>		(* single arg function *)
	     (lvar, LT.ltc_arrow(raw, [ltyArg], zs),
              L.FN (lvarArg, ltyArg, transLexp lexpBody))
       | _ => 
	     let
		 val (lvarArgs, ltyArgs) = ListPair.unzip formals
		 val lvarArg = LV.mkLvar()
		 val ltyArg = ltTuple ltyArgs
	     in
             (lvar, LT.ltc_arrow(raw, [ltyArg], zs),
		 L.FN (lvarArg, ltyArg,
		       selectLoop lvarArg lexpBody (0, lvarArgs)))
	     end)
    
and transLexp lexp =
    case lexp of
	F.RET [aValue] => 
	    L.SVAL (transVal aValue)

      | F.RET valueList => 
	    L.RECORD (map transVal valueList)

      | F.APP (funValue, [singleArg]) =>
	    L.APP (transVal funValue,
		   transVal singleArg)

      | F.APP (funValue, argList) => 
	    let val v = LV.mkLvar()
	    in
		L.LET (v, 
		       L.RECORD (map transVal argList),
		       L.APP (transVal funValue, L.VAR v))
	    end

      | F.TAPP (tfunValue, tycList) =>
	    L.TAPP (transVal tfunValue,
		    tycList)

      (* LET can be tricky.  If we're binding a singleton, it can be
       * translated directly.  If it's multiple vars and the binding 
       * expression is RET [v1,v2,v3] then we can do an iterated LET.
       * Otherwise, the binding function could be a function call or 
       * something with multiple values.  In this case, we will have 
       * to do a series of record selections.
       *)
      | F.LET ([lvar], lexpBinding, lexpBody) =>
	    let 
		val lexpBinding' = transLexp lexpBinding
	    in 
(* 		case lexpBinding' of  *)
(* 		    L.LET (lvar', lexp', lexpBody') => *)
(* 			L.LET (lvar', lexp', *)
(* 			       L.LET (lvar, lexpBody', *)
(* 				      transLexp lexpBody)) *)
(* 		  | _ =>  *)
		L.LET (lvar, lexpBinding',
		       transLexp lexpBody)
	    end

      | F.LET (lvarList, lexpBinding, lexpBody) => 
	    let
		val lvarRecord = LV.mkLvar();
		val lexpBinding' = transLexp lexpBinding
	    in
(* 		case lexpBinding' of *)
(* 		    L.LET (lvar', lexp', lexpBody') => *)
(* 			L.LET (lvar', lexp', *)
(* 			       L.LET (lvarRecord, lexpBody', *)
(* 				      selectLoop lvarRecord lexpBody  *)
(* 				      (0, lvarList))) *)
(* 		  | _ =>  *)
		L.LET (lvarRecord, lexpBinding',
		       selectLoop lvarRecord lexpBody (0, lvarList))
	    end

      | F.FIX ([x as (fk as {isrec=NONE,...},v,_,_)], lexpBody) => 
            L.LET(v, transFundec x, transLexp lexpBody)

      | F.FIX (fundecs, lexpBody) => 
	    let
		fun loop [] = ([], [], [])
		  | loop (fundec::fundecs) =
		    let
			val (lvar, lty, lexp) = transFundecRec fundec
			val (lvars, ltys, lexps) = loop fundecs
		    in
			(lvar::lvars, lty::ltys, lexp::lexps)
		    end

		val (lvars, ltys, lexps) = loop fundecs
	    in
		L.FIX (lvars, ltys, lexps, transLexp lexpBody)
	    end

      | F.TFN ((lvar, tformals, lexp), lexpBody) =>
	    let
		val kinds = map #2 tformals
	    in
		L.LET (lvar,
		       L.TFN (kinds, transLexp lexp),
		       transLexp lexpBody)
	    end

      | F.SWITCH (value, consig, conLexpList, lexpOpt) =>
	    let
		val value' = transVal value

		(* straight out of normalize.sml *)
		fun DECON'(dc as (_, A.REF, lt), ts, x) =
 		      L.APP (L.PRIM (PrimOp.DEREF, LT.lt_swap lt, ts), x)
                  | DECON'(dc as (_, A.SUSP(SOME(_, A.LVAR f)), lt), ts, x) = 
                      let val v = LV.mkLvar()
                       in L.LET(v, L.TAPP (L.VAR f, ts), L.APP (L.VAR v, x))
                      end
		  | DECON' z = L.DECON z

		fun tr (F.DATAcon (dcon, tycs, []), lexp) =
		    (L.DATAcon dcon, transLexp lexp)
		  | tr (F.DATAcon (dcon, tycs, [lvar]), lexp) =
		    (L.DATAcon dcon,
		     L.LET(lvar, DECON'(dcon, tycs, value'),
			   transLexp lexp))
		  | tr (F.DATAcon (dcon, tycs, lvars), lexp) =
		    let val v = LV.mkLvar()
		    in
			(L.DATAcon dcon,
			 L.LET (v, DECON'(dcon, tycs, value'),
				selectLoop v lexp (0, lvars)))
		    end
		  | tr (con, lexp) =
		    (transCon con, transLexp lexp)

		fun mapopt f NONE = NONE
		  | mapopt f (SOME x) = SOME (f x)
	    in
		L.SWITCH (value',
			  consig,
			  map tr conLexpList,
			  mapopt transLexp lexpOpt)
	    end

      | F.CON (dcon, tycs, [], lvar, lexp) => 
	    L.LET (lvar, L.CON (dcon, tycs, L.INT 0),
		   transLexp lexp)
      | F.CON (dcon, tycs, [value], lvar, lexp) => 
	    L.LET (lvar, L.CON (dcon, tycs, transVal value),
		   transLexp lexp)
      | F.CON (dcon, tycs, values, lvar, lexp) => 
	    let val v = LV.mkLvar()
	    in
		L.LET (v, L.RECORD (map transVal values),
		       L.LET (lvar, L.CON (dcon, tycs, L.VAR v),
			      transLexp lexp))
	    end

      | F.RECORD (F.RK_RECORD, values, lvar, lexp) => 
	    L.LET (lvar, L.RECORD (map transVal values), transLexp lexp)
      | F.RECORD (F.RK_STRUCT, values, lvar, lexp) => 
	    L.LET (lvar, L.SRECORD (map transVal values), transLexp lexp)
      | F.RECORD (F.RK_VECTOR tyc, values, lvar, lexp) => 
	    L.LET (lvar, L.VECTOR (map transVal values, tyc), transLexp lexp)

      | F.SELECT (value, i, lvar, lexp) => 
	    L.LET (lvar,
		   L.SELECT (i, transVal value),
		   transLexp lexp)

      | F.RAISE (value, ltys) => 
	    L.RAISE (transVal value, ltTuple ltys)

      | F.HANDLE (lexp, value) => 
	    L.HANDLE (transLexp lexp,
		      transVal value)

      | F.ETAG (tyc, value, lvar, lexp) => 
	    L.LET (lvar,
		   L.ETAG (transVal value,
			   LT.ltc_tyc tyc),
		   transLexp lexp)

      | F.PRIMOP (primop, [value], lvar, lexp) => 
	    L.LET (lvar, L.APP (L.PRIM primop, transVal value),
		   transLexp lexp)
      | F.PRIMOP (primop, values, lvar, lexp) => 
	    let val v = LV.mkLvar()
	    in
		L.LET (v, L.RECORD (map transVal values),
		       L.LET (lvar, L.APP (L.PRIM primop, L.VAR v),
			      transLexp lexp))
	    end

      | F.GENOP (dict, (po,lty,tycs), [value], lvar, lexp) => 
	    L.LET (lvar, L.APP (L.GENOP (dict,po,lty,tycs), transVal value),
		   transLexp lexp)
      | F.GENOP (dict, (po,lty,tycs), values, lvar, lexp) => 
	    let val v = LV.mkLvar()
	    in
		L.LET (v, L.RECORD (map transVal values),
		       L.LET (lvar, L.APP (L.GENOP (dict,po,lty,tycs), L.VAR v),
			      transLexp lexp))
	    end

(*
      | F.PACK (lty, tycs1, tycs2, v, lvar, lexp) =>
	    L.LET (lvar,
		   L.PACK (lty, tycs1, tycs2, transVal v),
		   transLexp lexp)
*)
      | F.WRAP (tyc, v, lvar, lexp) =>
	    L.LET (lvar,
		   L.WRAP (tyc, true, transVal v),
		   transLexp lexp)

      | F.UNWRAP (tyc, v, lvar, lexp) =>
	    L.LET (lvar,
		   L.UNWRAP (tyc, true, transVal v),
		   transLexp lexp)

end

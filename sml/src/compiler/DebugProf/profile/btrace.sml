(*
 * Perform Absyn annotations for back-tracing support.
 *   This adds a bt_add at the entry point of each FNexp,
 *   a push-restore sequence (bt_push) at each non-tail call site of
 *   a non-primitive function, and a save-restore sequence to each HANDLEexp.
 *
 *   Copyright (c) 2000 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure A = Absyn
    structure SE = StaticEnv
    structure SP = SymPath
    structure EM = ErrorMsg
    structure VC = VarCon
    structure BT = CoreBasicTypes
    structure AU = AbsynUtil

    structure Dummy = BTImp		(* mention it, so it gets made! *)
in

signature BTRACE = sig
    val enabled : bool ref
    val instrument :
	(Symbol.symbol -> bool) ->	(* isSpecial *)
	SE.staticEnv * A.dec CompInfo.compInfo -> A.dec -> A.dec
end

structure BTrace :> BTRACE = struct

    val priority = [10, 1]
    val obscurity = 1
    val prefix = "instrument"

    val registry = ControlRegistry.new { help = "instrumentation" }

    val _ = BasicControl.nest (prefix, registry)

    val bool_cvt = { tyName = "bool",
		     fromString = Bool.fromString,
		     toString = Bool.toString }

    val enabled = let
	val r = ref false
	val ctl = Controls.control { name = "btrace-mode",
				     pri = priority,
				     obscurity = obscurity,
				     help = "backtrace instrumentation mode",
				     ctl = r }
    in
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl bool_cvt ctl,
	      envName = SOME "INSTUMENT_BTRACE_MODE" };
	r
    end

    fun impossible s = EM.impossible ("BTrace: " ^ s)

    infix -->
    val op --> = BT.-->

    val i_i_Ty = BT.intTy --> BT.intTy
    val ii_u_Ty = BT.tupleTy [BT.intTy, BT.intTy] --> BT.unitTy
    val ii_u_u_Ty = ii_u_Ty --> BT.unitTy
    val u_u_Ty = BT.unitTy --> BT.unitTy
    val u_u_u_Ty = BT.unitTy --> u_u_Ty
    val iis_u_Ty = BT.tupleTy [BT.intTy, BT.intTy, BT.unitTy] --> BT.unitTy

    fun instrument0 isSpecial (senv, cinfo: A.dec CompInfo.compInfo) d = let

	val matchstring = #errorMatch cinfo

	val mkv = #mkLvar cinfo

	fun tmpvar (n, t) = let
	    val sy = Symbol.varSymbol n
	in
	    VC.VALvar { access = Access.namedAcc (sy, mkv), info = II.Null,
			path = SP.SPATH [sy], typ = ref t }
	end

(*
	val isSpecial = let
	    val l = [SpecialSymbols.paramId,
		     SpecialSymbols.functorId,
		     SpecialSymbols.hiddenId,
		     SpecialSymbols.tempStrId,
		     SpecialSymbols.tempFctId,
		     SpecialSymbols.fctbodyId,
		     SpecialSymbols.anonfsigId,
		     SpecialSymbols.resultId,
		     SpecialSymbols.returnId,
		     SpecialSymbols.internalVarId]
	in
	    fn s => List.exists (fn s' => Symbol.eq (s, s')) l
	end
*)
	     
	fun cons (s, []) = if isSpecial s then [] else [(s, 0)]
	  | cons (s, l as ((s', m) :: t)) =
	    if isSpecial s then l
	    else if Symbol.eq (s, s') then (s, m+1) :: t
	    else (s, 0) :: l

	fun getCoreVal s = CoreAccess.getVar (senv, s)
	fun getCoreCon s = CoreAccess.getCon (senv, s)

	val bt_reserve = getCoreVal "bt_reserve"
	val bt_register = getCoreVal "bt_register"
	val bt_save = getCoreVal "bt_save"
	val bt_push = getCoreVal "bt_push"
	val bt_nopush = getCoreVal "bt_nopush"
	val bt_add = getCoreVal "bt_add"
	val matchcon = getCoreCon "Match"

	val bt_register_var = tmpvar ("<bt_register>", iis_u_Ty)
	val bt_save_var = tmpvar ("<bt_save>", u_u_u_Ty)
	val bt_push_var = tmpvar ("<bt_push>", ii_u_u_Ty)
	val bt_nopush_var = tmpvar ("<bt_nopush>", ii_u_Ty)
	val bt_add_var = tmpvar ("<bt_add>", ii_u_Ty)
	val bt_reserve_var = tmpvar ("<bt_reserve>", i_i_Ty)
	val bt_module_var = tmpvar ("<bt_module>", BT.intTy)

	fun VARexp v = A.VARexp (ref v, [])
	fun INTexp i = A.INTexp (IntInf.fromInt i, BT.intTy)

	val uExp = AU.unitExp
	val pushexp = A.APPexp (VARexp bt_push_var, uExp)
	val saveexp = A.APPexp (VARexp bt_save_var, uExp)

	fun mkmodidexp fctvar id =
	    A.APPexp (VARexp fctvar,
		      AU.TUPLEexp [VARexp bt_module_var, INTexp id])

	val mkaddexp = mkmodidexp bt_add_var
	val mkpushexp = mkmodidexp bt_push_var
	val mknopushexp = mkmodidexp bt_nopush_var

	fun mkregexp (id, s) =
	    A.APPexp (VARexp bt_register_var,
		      AU.TUPLEexp [VARexp bt_module_var,
				   INTexp id, A.STRINGexp s])

	val regexps = ref []
	val next = ref 0

	fun newid s = let
	    val id = !next
	in
	    next := id + 1;
	    regexps := mkregexp (id, s) :: !regexps;
	    id
	end

	val mkadd = mkaddexp o newid
	val mkpush = mkpushexp o newid
	val mknopush = mknopushexp o newid

	fun VALdec (v, e) =
	    A.VALdec [A.VB { pat = A.VARpat v, exp = e,
			     tyvars = ref [], boundtvs = [] }]
	fun LETexp (v, e, b) = A.LETexp (VALdec (v, e), b)
	fun AUexp v = A.APPexp (VARexp v, uExp)	(* apply to unit *)

	fun is_prim_exp (A.VARexp (ref (VC.VALvar v), _)) =
	    II.isSimple (#info v)
	  | is_prim_exp (A.CONexp _) = true
	  | is_prim_exp (A.CONSTRAINTexp (e, _)) = is_prim_exp e
	  | is_prim_exp (A.MARKexp (e, _)) = is_prim_exp e
	  | is_prim_exp _ = false

	fun is_raise_exp (A.RAISEexp (e, _)) =
	    let fun is_simple_exn (A.VARexp _) = true
		  | is_simple_exn (A.CONexp _) = true
		  | is_simple_exn (A.CONSTRAINTexp (e, _)) = is_simple_exn e
		  | is_simple_exn (A.MARKexp (e, _)) = is_simple_exn e
		  | is_simple_exn (A.RAISEexp (e, _)) =
		    is_simple_exn e	(* !! *)
		  | is_simple_exn _ = false
	    in
		is_simple_exn e
	    end
	  | is_raise_exp (A.MARKexp (e, _) |
			  A.CONSTRAINTexp (e, _) |
			  A.SEQexp [e]) = is_raise_exp e
	  | is_raise_exp _ = false

	fun mkDescr ((n, r), what) = let
	    fun name ((s, 0), a) = Symbol.name s :: a
	      | name ((s, m), a) = Symbol.name s :: "[" ::
				   Int.toString (m + 1) :: "]" :: a
	    fun dot ([z], a) = name (z, a)
	      | dot (h :: t, a) = dot (t, "." :: name (h, a))
	      | dot ([], a) = impossible (what ^ ": no path")
	    val ms = matchstring r
	in
	    concat (ms :: ": " :: dot (n, []))
	end

	fun i_exp _ loc (A.RECORDexp l) =
	    A.RECORDexp (map (fn (l, e) => (l, i_exp false loc e)) l)
	  | i_exp _ loc (A.SELECTexp (l, e)) =
	    A.SELECTexp (l, i_exp false loc e)
	  | i_exp _ loc (A.VECTORexp (l, t)) =
	    A.VECTORexp (map (i_exp false loc) l, t)
	  | i_exp tail loc (A.PACKexp (e, t, tcl)) =
	    A.PACKexp (i_exp tail loc e, t, tcl)
	  | i_exp tail loc (e as A.APPexp (f, a)) = let
		val mainexp =  A.APPexp (i_exp false loc f, i_exp false loc a)
	    in
		if is_prim_exp f then mainexp
		else if tail then A.SEQexp [mknopush (mkDescr (loc, "GOTO")),
					    mainexp]
		else let
			val ty = Reconstruct.expType e
			val result = tmpvar ("tmpresult", ty)
			val restore = tmpvar ("tmprestore", u_u_Ty)
			val pushexp = mkpush (mkDescr (loc, "CALL"))
		    in
			LETexp (restore, pushexp,
				LETexp (result, mainexp,
					A.SEQexp [AUexp restore,
						  VARexp result]))
		    end
	    end
	  | i_exp tail loc (A.HANDLEexp (e, A.HANDLER (A.FNexp (rl, t)))) = let
		val restore = tmpvar ("tmprestore", u_u_Ty)
		fun rule (r as A.RULE (p, e)) =
		    if is_raise_exp e then r
		    else A.RULE (p, A.SEQexp [AUexp restore, i_exp tail loc e])
	    in
		LETexp (restore, saveexp,
			A.HANDLEexp (i_exp false loc e,
				     A.HANDLER (A.FNexp (map rule rl, t))))
	    end
	  | i_exp _ _ (A.HANDLEexp _) = impossible "bad HANDLEexp"
	  | i_exp _ loc (A.RAISEexp (e, t)) =
	    A.RAISEexp (i_exp false loc e, t)
	  | i_exp tail loc (A.CASEexp (e, rl, b)) =
	    A.CASEexp (i_exp false loc e, map (i_rule tail loc) rl, b)
	  | i_exp tail loc (A.FNexp (rl, t)) = let
		val addexp = mkadd (mkDescr (loc, "FN"))
		val arg = tmpvar ("fnvar", t)
		val rl' = map (i_rule true loc) rl
		val re = let
		    val A.RULE (_, lst) = List.last rl
		    val t = Reconstruct.expType lst
		in
		    A.RAISEexp (A.CONexp (matchcon, []), t)
		end
	    in
		A.FNexp ([A.RULE (A.VARpat arg,
				  A.SEQexp [addexp,
					    A.CASEexp (A.VARexp (ref arg, []),
						       rl', true)]),
			  A.RULE (A.WILDpat, re)],
			 t)
	    end
	  | i_exp tail loc (A.LETexp (d, e)) =
	    A.LETexp (i_dec loc d, i_exp tail loc e)
	  | i_exp tail loc (A.SEQexp l) =
	    A.SEQexp (#1 (foldr (fn (e, (l, t)) => (i_exp t loc e :: l, false))
				([], tail) l))
	  | i_exp tail loc (A.CONSTRAINTexp (e, t)) =
	    A.CONSTRAINTexp (i_exp tail loc e, t)
	  | i_exp tail (n, _) (A.MARKexp (e, r)) =
	    A.MARKexp (i_exp tail (n, r) e, r)
	  | i_exp _ _ e = e

	and i_dec loc (A.VALdec l) = A.VALdec (map (i_vb loc) l)
	  | i_dec loc (A.VALRECdec l) = A.VALRECdec (map (i_rvb loc) l)
	  | i_dec loc (A.ABSTYPEdec { abstycs, withtycs, body }) =
	    A.ABSTYPEdec { abstycs = abstycs, withtycs = withtycs,
			   body = i_dec loc body }
	  | i_dec loc (A.EXCEPTIONdec l) = A.EXCEPTIONdec (map (i_eb loc) l)
	  | i_dec loc (A.STRdec l) = A.STRdec (map (i_strb loc) l)
	  | i_dec loc (A.ABSdec l) = A.ABSdec (map (i_strb loc) l)
	  | i_dec loc (A.FCTdec l) = A.FCTdec (map (i_fctb loc) l)
	  | i_dec loc (A.LOCALdec (d, d')) =
	    A.LOCALdec (i_dec loc d, i_dec loc d')
	  | i_dec loc (A.SEQdec l) = A.SEQdec (map (i_dec loc) l)
	  | i_dec (n, _) (A.MARKdec (d, r)) = A.MARKdec (i_dec (n, r) d, r)
	  | i_dec _ d = d

	and i_rule tail loc (A.RULE (p, e)) = A.RULE (p, i_exp tail loc e)

	and i_vb (n, r) (vb as A.VB { pat, exp, boundtvs, tyvars }) = let
	    fun gv (A.VARpat v) = SOME v
	      | gv (A.CONSTRAINTpat (p, _)) = gv p
	      | gv (A.LAYEREDpat (p, p')) =
		(case gv p of
		     SOME v => SOME v
		   | NONE => gv p')
	      | gv _ = NONE
	    fun recur n = A.VB { pat = pat, exp = i_exp false (n, r) exp,
				 boundtvs = boundtvs, tyvars = tyvars }
	in
	    case gv pat of
		SOME (VC.VALvar { path = SP.SPATH [x], info, ... }) =>
		if II.isSimple info then vb
		else recur (cons (x, n))
	      | SOME (VC.VALvar { info, ... }) =>
		if II.isSimple info then vb else recur n
	      | _ => recur n
	end

	and i_rvb (n, r) (A.RVB { var, exp, boundtvs, resultty, tyvars }) = let
	    val x =
		case var of
		    VC.VALvar { path = SymPath.SPATH [x], ... } => x
		  | _ => impossible "VALRECdec"
	in
	    A.RVB { var = var, exp = i_exp false (cons (x, n), r) exp,
		    boundtvs = boundtvs, resultty = resultty, tyvars = tyvars }
	end

	and i_eb loc (A.EBgen { exn, etype, ident }) =
	    A.EBgen { exn = exn, etype = etype, ident = i_exp false loc ident }
	  | i_eb _ eb = eb

	and i_strb (n, r) (A.STRB { name, str, def }) =
	    A.STRB { name = name, str = str,
		     def = i_strexp (cons (name, n), r) def }

	and i_fctb (n, r) (A.FCTB { name, fct, def }) =
	    A.FCTB { name = name, fct = fct,
		     def = i_fctexp (cons (name, n), r) def }

	and i_strexp loc (A.LETstr (d, s)) =
	    A.LETstr (i_dec loc d, i_strexp loc s)
	  | i_strexp (n, _) (A.MARKstr (s, r)) =
	    A.MARKstr (i_strexp (n, r) s, r)
	  | i_strexp _ s = s

	and i_fctexp loc (A.FCTfct { param, argtycs, def }) =
	    A.FCTfct { param = param, argtycs = argtycs,
		       def = i_strexp loc def }
	  | i_fctexp loc (A.LETfct (d, f)) =
	    A.LETfct (i_dec loc d, i_fctexp loc f)
	  | i_fctexp (n, _) (A.MARKfct (f, r)) =
	    A.MARKfct (i_fctexp (n, r) f, r)
	  | i_fctexp _ f = f

	val d' = i_dec ([], (0, 0)) d
    in
	A.LOCALdec (A.SEQdec [VALdec (bt_reserve_var, AUexp bt_reserve),
			      VALdec (bt_module_var,
				      A.APPexp (VARexp bt_reserve_var,
						INTexp (!next))),
			      VALdec (bt_save_var, AUexp bt_save),
			      VALdec (bt_push_var, AUexp bt_push),
			      VALdec (bt_nopush_var, AUexp bt_nopush),
			      VALdec (bt_register_var, AUexp bt_register),
			      VALdec (bt_add_var,
				      A.SEQexp (!regexps @ [AUexp bt_add]))],
		    d')
    end

    fun instrument isSpecial params d =
	if !enabled then
	    instrument0 isSpecial params d
	    handle NoCore => d		(* this takes care of core.sml *)
	else d
end

end (* local *)

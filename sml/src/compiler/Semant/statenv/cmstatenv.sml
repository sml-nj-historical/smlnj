(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cmstatenv.sml *)

structure CMStaticEnv : CMSTATICENV = struct

    local
	structure M = Modules
	structure ED = EntPath.EvDict
	structure I = ModuleId
	structure V = VarCon
	structure T = Types
	structure B = Bindings
	structure SE = StaticEnv
    in

	val debugging = ref false	(* to keep signature happy *)

	(* -------------- *)

	structure Key = struct
	    type ord_key = I.modId
	    val cmpKey = I.cmp
	end

	structure D = BinaryDict (Key)

	type modmap = { strD : M.Structure D.dict,
		        sigD : M.Signature D.dict,
			fctD : M.Functor D.dict,
			tycD : T.tycon D.dict,
			eenvD : M.entityEnv D.dict }

	type modmaps = modmap list

	type staticEnv = SE.staticEnv * modmaps

	val emptyModmap : modmap =
	    { strD = D.mkDict (),
	      sigD = D.mkDict (),
	      fctD = D.mkDict (),
	      tycD = D.mkDict (),
	      eenvD = D.mkDict () }

	fun unCM (se, _) = se

	val empty = (StaticEnv.empty, [])

	(* don't worry about space leak -- it is not permanent *)
	fun atop ((se1, mm1), (se2, mm2)) = (SE.atop (se1, se2), mm1 @ mm2)

	fun adjCM (scsel, se) = let
	    fun layerAll [] = []
	      | layerAll [(_, mm)] = mm
	      | layerAll ((_, mm) :: r) = mm @ layerAll r
	in
	    (se, layerAll scsel)
	end

	fun mkLook sel (se: staticEnv) mid = let
		fun look [] = NONE
		  | look (mm :: mms) =
		    case D.peek (sel mm, mid) of
			NONE => look mms
		      | SOME x => SOME x
	in
	    look (#2 se)
	end

	val lookSTR = mkLook #strD
	val lookSIG = mkLook #sigD
	val lookFCT = mkLook #fctD
	val lookTYC = mkLook #tycD
	val lookEENV = mkLook #eenvD
	val lookFSIG = fn _ => fn _ => NONE

	exception Id

	fun strId (M.STR { rlzn = { stamp = rlznst, ... },
			   sign = M.SIG { stamp = sigst, ...}, ... }) =
	    I.STRid { rlzn = rlznst, sign = sigst }
	  | strId _ = raise Id

	fun fsigId(M.FSIG{paramsig=M.SIG{stamp=sp,...},
			  bodysig=M.SIG{stamp=sb,...},...}) =
	    I.FSIGid{paramsig=sp,bodysig=sb}
	  | fsigId _ = raise Id

	fun fctId(M.FCT{rlzn={stamp,...},sign,...}) =
	    I.FCTid{rlzn=stamp,sign=fsigId sign}
	  | fctId _ = raise Id

	fun addSTR (i, b) { strD, sigD, fctD, tycD, eenvD } =
	    { strD = D.insert (strD, i, b),
	      sigD = sigD, fctD = fctD, tycD = tycD, eenvD = eenvD }
	fun addSIG (i, b) { strD, sigD, fctD, tycD, eenvD } =
	    { strD = strD,
	      sigD = D.insert (sigD, i, b),
	      fctD = fctD, tycD = tycD, eenvD = eenvD }
	fun addFCT (i, b) { strD, sigD, fctD, tycD, eenvD } =
	    { strD = strD, sigD = sigD,
	      fctD = D.insert (fctD, i, b),
	      tycD = tycD, eenvD = eenvD }
	fun addTYC (i, b) { strD, sigD, fctD, tycD, eenvD } =
	    { strD = strD, sigD = sigD, fctD = fctD,
	      tycD = D.insert (tycD, i, b),
	      eenvD = eenvD }
	fun addEENV (i, b) { strD, sigD, fctD, tycD, eenvD } =
	    { strD = strD, sigD = sigD, fctD = fctD, tycD = tycD,
	      eenvD = D.insert (eenvD, i, b) }

	fun enter (sel, add) (i, b, go_inside) (table: modmap) =
	    case D.peek (sel table, i) of
		SOME _ => table
	      | NONE => go_inside (add (i, b) table)

	val enterSTR = enter (#strD, addSTR)
	val enterSIG = enter (#sigD, addSIG)
	val enterFCT = enter (#fctD, addFCT)
	val enterTYC = enter (#tycD, addTYC)
	val enterEENV = enter (#eenvD, addEENV)

	fun nothing table = table
	fun list x = foldr (op o) nothing x

	fun getbindings env = SE.fold (fn ((s, b), l) => b :: l) nil env

	fun binding (B.VALbind v) = var v
	  | binding (B.CONbind v) = datacon v
	  | binding (B.TYCbind v) = tycon v
	  | binding (B.SIGbind v) = Signature v
	  | binding (B.STRbind v) = Structure v
	  | binding (B.FSGbind v) = fctSig v
	  | binding (B.FCTbind v) = Functor v
	  | binding (B.FIXbind v) = nothing

	and var (V.VALvar { typ = ref t, ...}) = ty t
	  | var (V.OVLDvar { options = ref p, scheme = s, ...}) = 
	    (list (map var_option p)  o  tyfun s)
	  | var (V.ERRORvar) = nothing

	and var_option {indicator, variant} = ty indicator  o  var variant

	and tyfun (T.TYFUN { body, ...}) = ty body

	and ty (T.VARty (ref (T.INSTANTIATED t))) = ty t
	  | ty (T.CONty (tyc, tyl)) = (tycon tyc o list (map ty tyl))
	  | ty (T.POLYty { tyfun = t, ...}) = tyfun t
	  | ty _ = nothing

	and tycon (t as T.GENtyc { stamp, ... }) =
	    enterTYC (I.TYCid stamp, t, nothing)
	  | tycon (T.DEFtyc { tyfun = t, ... }) = tyfun t
	  | tycon _ = nothing

	and datacon (T.DATACON {typ, ... }) = ty typ

	and spec (M.TYCspec { spec = t, ... }) = tycon t
	  | spec (M.STRspec { sign = s, ... }) = Signature s
	  | spec (M.FCTspec { sign = s, ... }) = fctSig s
	  | spec (M.VALspec { spec = t, ... }) = ty t
	  | spec (M.CONspec { spec = d, ... }) = datacon d

	and Signature (s as M.SIG { stamp, elements = e, ... }) =
	    enterSIG (I.SIGid stamp, s, fn x => list (map (spec o #2) e) x)
	  | Signature M.ERRORsig = nothing

	and Structure (s as M.STR { sign = g, rlzn = r, ... }) =
	    enterSTR (strId s, s, fn x => (Signature g o strEntity r) x)
	  | Structure (M.STRSIG { sign = s, ... }) = Signature s
	  | Structure (M.ERRORstr) = nothing

	and tycExp (M.CONSTtyc t) = tycon t
	  | tycExp _ = nothing

	and strExp (M.VARstr _) = nothing
	  | strExp (M.CONSTstr strent) = strEntity strent
	  | strExp (M.STRUCTURE { entDec = d, ... }) = entityDec d
	  | strExp (M.APPLY (f, s)) = fctExp f o strExp s
	  | strExp (M.LETstr (d, e)) = entityDec d o strExp e
	  | strExp (M.ABSstr (s, e)) = Signature s o strExp e  
	  | strExp (M.CONSTRAINstr { raw, coercion, ... }) =
	    strExp raw o strExp coercion
	  | strExp (M.FORMstr fs) = fctSig fs

	and fctExp (M.VARfct _) = nothing
	  | fctExp (M.CONSTfct f) = fctEntity f
	  | fctExp (M.LAMBDA { body, ... }) = strExp body
	  | fctExp (M.LAMBDA_TP { body, sign, ... }) =
	    strExp body o fctSig sign
	  | fctExp (M.LETfct (d, f)) = entityDec d o fctExp f

	and entityDec (M.TYCdec (v, t)) = tycExp t
	  | entityDec (M.STRdec (v, s, _)) = strExp s
	  | entityDec (M.FCTdec(v,f)) = fctExp f
	  | entityDec (M.SEQdec ds) = list (map entityDec ds)
	  | entityDec (M.LOCALdec (din, dout)) = entityDec din o entityDec dout
	  | entityDec (M.ERRORdec) = nothing
	  | entityDec (M.EMPTYdec) = nothing

	and strEntity { entities = e, ...} = entityEnv e

	and fctEntity { closure = M.CLOSURE { body = b, env = e, ... }, ... } =
	    strExp b  o  entityEnv e

	and tycEntity t = tycon t

	and entityEnv (e as M.MARKeenv(s, rest)) =
	    enterEENV (I.EENVid s, e, fn x => entityEnv rest x)
	  | entityEnv (M.BINDeenv (d, rest)) =
	    (list (map (entity o #2) (ED.members d))) o entityEnv rest
	  | entityEnv M.NILeenv = nothing
	  | entityEnv M.ERReenv = nothing

	and entity(M.TYCent t) = tycEntity t
	  | entity(M.STRent s) = strEntity s
	  | entity(M.FCTent f) = fctEntity f
	  | entity _ = nothing

	and fctSig (M.FSIG { paramsig = p, bodysig = b, ... }) =
	    Signature p o Signature b
	  | fctSig M.ERRORfsig = nothing

	and Functor (f as M.FCT { sign = s, rlzn = r, ...}) =
	    enterFCT (fctId f, f, fn x => (fctSig s o fctEntity r) x)
	  | Functor M.ERRORfct = nothing

	and env e = list (map binding (getbindings e))

	fun CM env0 = (env0, [env env0 emptyModmap])

	val CM = Stats.doPhase (Stats.makePhase "Compiler 038 cmstatenv") CM

	fun consolidate (e, _) = CM (SE.consolidate e)
    end (* local *)
end (* structure CMStaticEnv *)


(*
 * $Log$
 *)



(*
 * The revised pickler using the new "generic" pickling facility.
 *
 * July 1999, Matthias Blume
 *)
signature PICKMOD = sig

    datatype ckey =			(* context key *)
	PrimKey				(* for primEnv *)
      | NodeKey of int * Symbol.symbol	(* n-th sublib, module exporting sym *)

    type 'a context =
	{ lookSTR: ModuleId.modId -> 'a,
	  lookSIG: ModuleId.modId -> 'a,
	  lookFCT: ModuleId.modId -> 'a,
	  lookFSIG: ModuleId.modId -> 'a,
	  lookTYC: ModuleId.modId -> 'a,
	  lookEENV: ModuleId.modId -> 'a }

    (* All we really need here is a "bool context", but passing the whole
     * CMStaticEnv.staticEnv is more convenient (and backward-compatible). *)
    val pickleEnv :
	{ context: CMStaticEnv.staticEnv, env: StaticEnv.staticEnv  }
	-> { hash: PersStamps.persstamp,
	     pickle: Word8Vector.vector, 
	     exportLvars: Access.lvar list,
	     exportPid: PersStamps.persstamp option }

    (* Re-pickling is done for the purpose of getting the hash value
     * of a "reduced" (filtered) version of another environment that
     * has been pickled before.  During re-pickling, the LOCAL->GLOBAL
     * translation for stamps and the LVAR->EXTERN translation for
     * accesses is undone so that the resulting hash value is the
     * same that one would have gotten if the current environment
     * was pickled using "pickleEnv". The context for repickling is
     * specified using a set of module IDs instead of an entire
     * context environment.  The set will have to be obtained from the
     * unpickling process of the original pickle. *)
    val repickleEnvHash :
	{ context: ModuleId.Set.set,
	  env: StaticEnv.staticEnv,
	  orig_hash: PersStamps.persstamp } -> PersStamps.persstamp

    val pickleFLINT:
        CompBasic.flint option
	-> { hash: PersStamps.persstamp,
	     pickle: Word8Vector.vector }

    (* The following is low-level interface so this pickler can be part
     * of another, bigger, pickler. *)
    type map
    val emptyMap : map

    type env'n'ctxt = { env: StaticEnv.staticEnv, ctxt: ModuleId.Set.set }

    val envPickler :
	ckey option context -> (map, env'n'ctxt) PickleUtil.pickler

    val symenvPickler : (map, SymbolicEnv.symenv) PickleUtil.pickler

    val pickle2hash: Word8Vector.vector -> PersStamps.persstamp
	
    val dontPickle : 
        StaticEnv.staticEnv * int
                  -> StaticEnv.staticEnv * PersStamps.persstamp *
	             Access.lvar list * PersStamps.persstamp option
end

local
    (* make those into red-black-maps once rb-maps work correcty. *)
    functor MapFn = RedBlackMapFn
    structure IntMap = IntRedBlackMap
in
  structure PickMod :> PICKMOD = struct

    (* to gather some statistics... *)
    val addPickles = Stats.addStat (Stats.makeStat "Pickle Bytes")

    fun bug msg = ErrorMsg.impossible ("PickMod: " ^ msg)

    structure A = Access
    structure DI = DebIndex
    structure LK = LtyKernel
    structure PT = PrimTyc
    structure F = FLINT
    structure T = Types
    structure SP = SymPath
    structure IP = InvPath
    structure MI = ModuleId
    structure II = InlInfo
    structure V = VarCon
    structure ED = EntPath.EvDict
    structure PS = PersStamps
    structure P = PrimOp
    structure M = Modules
    structure B = Bindings

    (** NOTE: the CRC functions really ought to work on Word8Vector.vectors **)
    fun pickle2hash pickle =
	PS.fromBytes
	  (Byte.stringToBytes
	     (CRC.toString
	        (CRC.fromString
		  (Byte.bytesToString pickle))))

    fun symCmp (a, b) =
	if Symbol.symbolGt (a, b) then GREATER
	else if Symbol.eq (a, b) then EQUAL else LESS

    structure LTMap = MapFn
	(struct type ord_key = LK.lty val compare = LK.lt_cmp end)
    structure TCMap = MapFn
	(struct type ord_key = LK.tyc val compare = LK.tc_cmp end)
    structure TKMap = MapFn
	(struct type ord_key = LK.tkind val compare = LK.tk_cmp end)
    local
	structure StampMap = MapFn
	    (struct type ord_key = Stamps.stamp val compare = Stamps.cmp end)
    in
	structure DTMap = StampMap
	structure MBMap = StampMap
    end


    type pid = PS.persstamp
    type mi = MI.modId * pid option

    fun mi_cmp ((mi, po), (mi', po')) = let
	fun po_cmp (NONE, NONE) = EQUAL
	  | po_cmp (NONE, SOME _) = LESS
	  | po_cmp (SOME _, NONE) = GREATER
	  | po_cmp (SOME p, SOME p') = PS.compare (p, p')
    in
	case MI.cmp (mi, mi') of
	    EQUAL => po_cmp (po, po')
	  | unequal => unequal
    end

    fun acc_pid (A.LVAR _) = NONE
      | acc_pid (A.EXTERN p) = SOME p
      | acc_pid (A.PATH (a, _)) = acc_pid a
      | acc_pid A.NO_ACCESS = NONE

    structure MIMap = MapFn
	(struct type ord_key = mi val compare = mi_cmp end)

    structure PU = PickleUtil
    structure PSymPid = PickleSymPid

    type map =
	{ lt: PU.id LTMap.map,
	  tc: PU.id TCMap.map,
	  tk: PU.id TKMap.map,
	  dt: PU.id DTMap.map,
	  mb: PU.id MBMap.map,
	  mi: PU.id MIMap.map }

    val emptyMap = { lt = LTMap.empty, tc = TCMap.empty, tk = TKMap.empty,
		     dt = DTMap.empty, mb = MBMap.empty, mi = MIMap.empty }

    (* type info *)
    val (NK, AO, CO, PO, CS, A, CR, LT, TC, TK,
	 V, C, E, FK, RK, ST, MI, EQP, TYCKIND, DTI,
	 DTF, TYCON, T, II, VAR, SD, SG, FSG,  SP, EN,
	 STR, F, STE, TCE, STRE, FE, EE, ED, EEV, FX,
	 B, DCON, DICT, FPRIM, FUNDEC, TFUNDEC, DATACON, DTMEM, NRD,
	 OVERLD, FCTC, SEN, FEN, SPATH, IPATH) =
	(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
	 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
	 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
	 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
	 41, 42, 43, 44, 45, 46, 47, 48, 49,
	 50, 51, 52, 53, 54, 55)

    (* this is a bit awful...
     * (we really ought to have syntax for "functional update") *)
    val LTs = { find = fn (m: map, x) => LTMap.find (#lt m, x),
	        insert = fn ({ lt, tc, tk, dt, mb, mi }, x, v) =>
		         { lt = LTMap.insert (lt, x, v),
			   tc = tc,
			   tk = tk,
			   dt = dt,
			   mb = mb,
			   mi = mi } }
    val TCs = { find = fn (m: map, x) => TCMap.find (#tc m, x),
	        insert = fn ({ lt, tc, tk, dt, mb, mi }, x, v) =>
		         { lt = lt,
			   tc = TCMap.insert (tc, x, v),
			   tk = tk,
			   dt = dt,
			   mb = mb,
			   mi = mi } }
    val TKs = { find = fn (m: map, x) => TKMap.find (#tk m, x),
	        insert = fn ({ lt, tc, tk, dt, mb, mi }, x, v) =>
		         { lt = lt,
			   tc = tc,
			   tk = TKMap.insert (tk, x, v),
			   dt = dt,
			   mb = mb,
			   mi = mi } }
    fun DTs x = { find = fn (m: map, _) => DTMap.find (#dt m, x),
		  insert = fn ({ lt, tc, tk, dt, mb, mi }, _, v) =>
		           { lt = lt,
			     tc = tc,
			     tk = tk,
			     dt = DTMap.insert (dt, x, v),
			     mb = mb,
			     mi = mi } }
    fun MBs x = { find = fn (m: map, _) => MBMap.find (#mb m, x),
		  insert = fn ({ lt, tc, tk, dt, mb, mi }, _, v) =>
		           { lt = lt,
			     tc = tc,
			     tk = tk,
			     dt = dt,
			     mb = MBMap.insert (mb, x, v),
			     mi = mi } }
    fun MIs x = { find = fn (m: map, _) => MIMap.find (#mi m, x),
		  insert = fn ({ lt, tc, tk, dt, mb, mi }, _, v) =>
		           { lt = lt,
			     tc = tc,
			     tk = tk,
			     dt = dt,
			     mb = mb,
			     mi = MIMap.insert (mi, x, v) } }

    infix 3 $

    val int = PU.w_int
    val int32 = PU.w_int32
    val word = PU.w_word
    val word32 = PU.w_word32
    val string = PU.w_string
    val share = PU.ah_share
    val list = PU.w_list
    val pair = PU.w_pair
    val bool = PU.w_bool
    val option = PU.w_option
    val symbol = PSymPid.w_symbol
    val pid = PSymPid.w_pid

    fun mkAlphaConvert () = let
	val m = ref IntMap.empty
	val cnt = ref 0
	fun cvt i =
	    case IntMap.find (!m, i) of
		SOME i' => i'
	      | NONE => let
		    val i' = !cnt
		in
		    cnt := i' + 1;
		    m := IntMap.insert (!m, i, i');
		    i'
		end
    in
	cvt
    end

    fun numkind arg = let
	val op $ = PU.$ NK
	fun nk (P.INT i) = "A" $ [int i]
	  | nk (P.UINT i) = "B" $ [int i]
	  | nk (P.FLOAT i) = "C" $ [int i]
    in
	nk arg
    end

    fun arithop oper = let
	val op $ = PU.$ AO
	fun arithopc P.+ = "\000"
	  | arithopc P.- = "\001"
	  | arithopc P.* = "\002"
	  | arithopc P./ = "\003"
	  | arithopc P.~ = "\004"
	  | arithopc P.ABS = "\005"
	  | arithopc P.LSHIFT = "\006"
	  | arithopc P.RSHIFT = "\007"
	  | arithopc P.RSHIFTL = "\008"
	  | arithopc P.ANDB = "\009"
	  | arithopc P.ORB = "\010"
	  | arithopc P.XORB = "\011"
	  | arithopc P.NOTB = "\012"
    in
	arithopc oper $ []
    end

    fun cmpop oper = let
	val op $ = PU.$ CO
	fun cmpopc P.> = "\000"
	  | cmpopc P.>= = "\001"
	  | cmpopc P.< = "\002"
	  | cmpopc P.<= = "\003"
	  | cmpopc P.LEU = "\004"
	  | cmpopc P.LTU = "\005"
	  | cmpopc P.GEU = "\006"
	  | cmpopc P.GTU = "\007"
	  | cmpopc P.EQL = "\008"
	  | cmpopc P.NEQ = "\009"
    in
	cmpopc oper $ []
    end
	    
    fun primop p = let
	val op $ = PU.$ PO
	fun ?n = String.str (Char.chr n)
	fun fromto tag (from, to) = ?tag $ [int from, int to]
	fun %?n = ?n $ []
	in
	    case p of
		P.ARITH { oper, overflow, kind } =>
		    ?100 $ [arithop oper, bool overflow, numkind kind]
	      | P.CMP { oper, kind } => ?101 $ [cmpop oper, numkind kind]
	      | P.TEST x => fromto 102 x
	      | P.TESTU x => fromto 103 x
	      | P.TRUNC x => fromto 104 x
	      | P.EXTEND x => fromto 105 x
	      | P.COPY x => fromto 106 x
	      | P.INLLSHIFT kind => ?107 $ [numkind kind]
	      | P.INLRSHIFT kind => ?108 $ [numkind kind]
	      | P.INLRSHIFTL kind => ?109 $ [numkind kind]
	      | P.ROUND { floor, fromkind, tokind } =>
		    ?110 $ [bool floor, numkind fromkind, numkind tokind]
	      | P.REAL { fromkind, tokind } =>
		    ?111 $ [numkind fromkind, numkind tokind]
	      | P.NUMSUBSCRIPT { kind, checked, immutable } =>
		    ?112 $ [numkind kind, bool checked, bool immutable]
	      | P.NUMUPDATE { kind, checked } =>
		    ?113 $ [numkind kind, bool checked]
	      | P.INL_MONOARRAY kind => ?114 $ [numkind kind]
	      | P.INL_MONOVECTOR kind => ?115 $ [numkind kind]
		    
	      | P.MKETAG => %?0
	      | P.WRAP => %?1
	      | P.UNWRAP => %?2
	      | P.SUBSCRIPT => %?3
	      | P.SUBSCRIPTV => %?4
	      | P.INLSUBSCRIPT => %?5
	      | P.INLSUBSCRIPTV => %?6
	      | P.INLMKARRAY => %?7
		    
	      | P.PTREQL => %?8
	      | P.PTRNEQ => %?9
	      | P.POLYEQL => %?10
	      | P.POLYNEQ => %?11
	      | P.BOXED => %?12
	      | P.UNBOXED => %?13
	      | P.LENGTH => %?14
	      | P.OBJLENGTH => %?15
	      | P.CAST => %?16
	      | P.GETRUNVEC => %?17
	      | P.MARKEXN => %?18
	      | P.GETHDLR => %?19
	      | P.SETHDLR => %?20
	      | P.GETVAR => %?21
	      | P.SETVAR => %?22
	      | P.GETPSEUDO => %?23
	      | P.SETPSEUDO => %?24
	      | P.SETMARK => %?25
	      | P.DISPOSE => %?26
	      | P.MAKEREF => %?27
	      | P.CALLCC => %?28
	      | P.CAPTURE => %?29
	      | P.THROW => %?30
	      | P.DEREF => %?31
	      | P.ASSIGN => %?32
	      (* NOTE: P.UNBOXEDASSIGN is defined below *)
	      | P.UPDATE => %?33
	      | P.INLUPDATE => %?34
	      | P.BOXEDUPDATE => %?35
	      | P.UNBOXEDUPDATE => %?36

	      | P.GETTAG => %?37
	      | P.MKSPECIAL => %?38
	      | P.SETSPECIAL => %?39
	      | P.GETSPECIAL => %?40
	      | P.USELVAR => %?41
	      | P.DEFLVAR => %?42
	      | P.INLDIV => %?43
	      | P.INLMOD => %?44
	      | P.INLREM => %?45
	      | P.INLMIN => %?46
	      | P.INLMAX => %?47
	      | P.INLABS => %?48
	      | P.INLNOT => %?49
	      | P.INLCOMPOSE => %?50
	      | P.INLBEFORE => %?51
	      | P.INL_ARRAY => %?52
	      | P.INL_VECTOR => %?53
	      | P.ISOLATE => %?54
	      | P.WCAST => %?55
	      | P.NEW_ARRAY0 => %?56
	      | P.GET_SEQ_DATA => %?57
	      | P.SUBSCRIPT_REC => %?58
	      | P.SUBSCRIPT_RAW64 => %?59
	      | P.UNBOXEDASSIGN => %?60
    end

    fun consig arg = let
	val op $ = PU.$ CS
	fun cs (A.CSIG (i, j)) = "S" $ [int i, int j]
	  | cs A.CNIL = "N" $ []
    in
	cs arg
    end

    fun mkAccess { lvar, isLocalPid } = let
	val op $ = PU.$ A
	fun access (A.LVAR i) = "A" $ [lvar i]
	  | access (A.EXTERN p) = "B" $ [pid p]
	  | access (A.PATH (a as A.EXTERN p, i)) =
	    (* isLocalPid always returns false for in the "normal pickler"
	     * case.  It returns true in the "repickle" case for the
	     * pid that was the hash of the original whole pickle.
	     * Since alpha-conversion has already taken place if we find
	     * an EXTERN pid, we don't call "lvar" but "int". *)
	    if isLocalPid p then "A" $ [int i]
	    else "C" $ [access a, int i]
	  | access (A.PATH (a, i)) = "C" $ [access a, int i]
	  | access A.NO_ACCESS = "D" $ []

	val op $ = PU.$ CR
	fun conrep A.UNTAGGED = "A" $ []
	  | conrep (A.TAGGED i) = "B" $ [int i]
	  | conrep A.TRANSPARENT = "C" $ []
	  | conrep (A.CONSTANT i) = "D" $ [int i]
	  | conrep A.REF = "E" $ []
	  | conrep (A.EXN a) = "F" $ [access a]
	  | conrep A.LISTCONS = "G" $ []
	  | conrep A.LISTNIL = "H" $ []
	  | conrep (A.SUSP NONE) = "I" $ []
	  | conrep (A.SUSP (SOME (a, b))) = "J" $ [access a, access b]
    in
	{ access = access, conrep = conrep }
    end

    (* lambda-type stuff; some of it is used in both picklers *)
    fun tkind x = let
	val op $ = PU.$ TK
	fun tk x =
	    case LK.tk_out x of
	    LK.TK_MONO => "A" $ []
	  | LK.TK_BOX => "B" $ []
	  | LK.TK_SEQ ks => "C" $ [list tkind ks]
	  | LK.TK_FUN (ks, kr) => "D" $ [list tkind ks, tkind kr]
    in
	share TKs tk x
    end

    fun mkLty lvar = let
	fun lty x = let
	    val op $ = PU.$ LT
	    fun ltyI x =
		case LK.lt_out x of
		    LK.LT_TYC tc => "A" $ [tyc tc]
		  | LK.LT_STR l => "B" $ [list lty l]
		  | LK.LT_FCT (ts1, ts2) => "C" $ [list lty ts1, list lty ts2]
		  | LK.LT_POLY (ks, ts) => "D" $ [list tkind ks, list lty ts]
		  | LK.LT_IND _ => bug "unexpected LT_IND in mkPickleLty"
		  | LK.LT_ENV _ => bug "unexpected LT_ENV in mkPickleLty"
		  | LK.LT_CONT _ => bug "unexpected LT_CONT in mkPickleLty"
	in
	    share LTs ltyI x
	end

	and tyc x = let
	    val op $ = PU.$ TC
	    fun tycI x =
		case LK.tc_out x of
		    LK.TC_VAR (db, i) => "A" $ [int (DI.di_toint db), int i]
		  | LK.TC_NVAR n => "B" $ [lvar n]
		  | LK.TC_PRIM t => "C" $ [int (PT.pt_toint t)]
		  | LK.TC_FN (ks, tc) => "D" $ [list tkind ks, tyc tc]
		  | LK.TC_APP (tc, l) => "E" $ [tyc tc, list tyc l]
		  | LK.TC_SEQ l => "F" $ [list tyc l]
		  | LK.TC_PROJ (tc, i) => "G" $ [tyc tc, int i]
		  | LK.TC_SUM l => "H" $ [list tyc l]
		  | LK.TC_FIX ((n, tc, ts), i) =>
			"I" $ [int n, tyc tc, list tyc ts, int i]
		  | LK.TC_ABS tc => "J" $ [tyc tc]
		  | LK.TC_BOX tc => "K" $ [tyc tc]
		  | LK.TC_TUPLE (_, l) => "L" $ [list tyc l]
		  | LK.TC_ARROW (LK.FF_VAR (b1, b2), ts1, ts2) =>
			"M" $ [bool b1, bool b2, list tyc ts1, list tyc ts2]
		  | LK.TC_ARROW (LK.FF_FIXED, ts1, ts2) =>
			"N" $ [list tyc ts1, list tyc ts2]
		  | LK.TC_PARROW _ => bug "unexpected TC_PARREW in mkPickleLty"
		  | LK.TC_TOKEN (tk, t) => "O" $ [int (LK.token_int tk), tyc t]
		  | LK.TC_IND _ => bug "unexpected TC_IND in mkPickleLty"
		  | LK.TC_ENV _ => bug "unexpected TC_ENV in mkPickleLty"
		  | LK.TC_CONT _ => bug "unexpected TC_CONT in mkPickleLty"
	in
	    share TCs tycI x
	end
    in
	{ tyc = tyc, lty = lty }
    end

    (* the FLINT pickler *)
    fun flint flint_exp = let
	val alphaConvert = mkAlphaConvert ()
	val lvar = int o alphaConvert
	val { access, conrep } = mkAccess { lvar = lvar,
					    isLocalPid = fn _ => false }
	val { lty, tyc } = mkLty lvar

	val op $ = PU.$ V
	fun value (F.VAR v) = "a" $ [lvar v]
	  | value (F.INT i) = "b" $ [int i]
	  | value (F.INT32 i32) = "c" $ [int32 i32]
	  | value (F.WORD w) = "d" $ [word w]
	  | value (F.WORD32 w32) = "e" $ [word32 w32]
	  | value (F.REAL s) = "f" $ [string s]
	  | value (F.STRING s) = "g" $ [string s]

	fun con arg = let
	    val op $ = PU.$ C
	    fun c (F.DATAcon (dc, ts, v), e) =
		"1" $ [dcon (dc, ts), lvar v, lexp e]
	      | c (F.INTcon i, e) = "2" $ [int i, lexp e]
	      | c (F.INT32con i32, e) = "3" $ [int32 i32, lexp e]
	      | c (F.WORDcon w, e) = "4" $ [word w, lexp e]
	      | c (F.WORD32con w32, e) = "5" $ [word32 w32, lexp e]
	      | c (F.REALcon s, e) = "6" $ [string s, lexp e]
	      | c (F.STRINGcon s, e) = "7" $ [string s, lexp e]
	      | c (F.VLENcon i, e) = "8" $ [int i, lexp e]
	in
	    c arg
	end

	and dcon ((s, cr, t), ts) = let
	    val op $ = PU.$ DCON
	in
	    "x" $ [symbol s, conrep cr, lty t, list tyc ts]
	end

	and dict { default = v, table = tbls } = let
	    val op $ = PU.$ DICT
	in
	    "y" $ [lvar v, list (pair (list tyc, lvar)) tbls]
	end

	and fprim (dtopt, p, t, ts) = let
	    val op $ = PU.$ FPRIM
	in
	    "z" $ [option dict dtopt, primop p, lty t, list tyc ts]
	end

	and lexp arg = let
	    val op $ = PU.$ E
	    fun l (F.RET vs) = "j" $ [list value vs]
	      | l (F.LET (vs, e1, e2)) =
		"k" $ [list lvar vs, lexp e1, lexp e2]
	      | l (F.FIX (fdecs, e)) = "l" $ [list fundec fdecs, lexp e]
	      | l (F.APP (v, vs)) = "m" $ [value v, list value vs]
	      | l (F.TFN (tfdec, e)) = "n" $ [tfundec tfdec, lexp e]
	      | l (F.TAPP (v, ts)) = "o" $ [value v, list tyc ts]
	      | l (F.SWITCH (v, crl, cel, eo)) =
		"p" $ [value v, consig crl, list con cel, option lexp eo]
	      | l (F.CON (dc, ts, u, v, e)) =
		"q" $ [dcon (dc, ts), value u, lvar v, lexp e]
	      | l (F.RECORD (rk, vl, v, e)) =
		"r" $ [rkind rk, list value vl, lvar v, lexp e]
	      | l (F.SELECT (u, i, v, e)) =
		"s" $ [value u, int i, lvar v, lexp e]
	      | l (F.RAISE (u, ts)) = "t" $ [value u, list lty ts]
	      | l (F.HANDLE (e, u)) = "u" $ [lexp e, value u]
	      | l (F.BRANCH (p, vs, e1, e2)) =
		"v" $ [fprim p, list value vs, lexp e1, lexp e2]
	      | l (F.PRIMOP (p, vs, v, e)) =
		"w" $ [fprim p, list value vs, lvar v, lexp e]
	in
	    l arg
	end

	and fundec (fk, v, vts, e) = let
	    val op $ = PU.$ FUNDEC
	in
	    "a" $ [fkind fk, lvar v, list (pair (lvar, lty)) vts, lexp e]
	end

	and tfundec (_, v, tvks, e) = let
	    val op $ = PU.$ TFUNDEC
	in
	    "b" $ [lvar v, list (pair (lvar, tkind)) tvks, lexp e]
	end

	and fkind arg = let
	    val op $ = PU.$ FK
	    fun isAlways F.IH_ALWAYS = true
	      | isAlways _ = false
	    fun strip (x, y) = x
	    fun fk { cconv = F.CC_FCT, ... } = "2" $ []
	      | fk { isrec, cconv = F.CC_FUN fixed, known, inline } =
		case fixed of
		    LK.FF_VAR (b1, b2) =>
			"3" $ [option (list lty) (Option.map strip isrec),
			       bool b1, bool b2, bool known,
			       bool (isAlways inline)]
		  | LK.FF_FIXED =>
			"4" $ [option (list lty) (Option.map strip isrec),
			       bool known, bool (isAlways inline)]
	in
	    fk arg
	end

	and rkind arg = let
	    val op $ = PU.$ RK
	    fun rk (F.RK_VECTOR tc) = "5" $ [tyc tc]
	      | rk F.RK_STRUCT = "6" $ []
	      | rk (F.RK_TUPLE _) = "7" $ []
	in
	    rk arg
	end
    in
	fundec flint_exp
    end

    fun pickleFLINT fo = let
	val pickle =
	    Byte.stringToBytes (PU.pickle emptyMap (option flint fo))
	val hash = pickle2hash pickle
    in
	{ pickle = pickle, hash = hash }
    end

    fun symenvPickler sye =
	list (pair (pid, flint)) (SymbolicEnv.listItemsi sye)

    datatype ckey =			(* context key *)
	PrimKey
      | NodeKey of int * Symbol.symbol

    type 'a context =
	{ lookSTR: ModuleId.modId -> 'a,
	  lookSIG: ModuleId.modId -> 'a,
	  lookFCT: ModuleId.modId -> 'a,
	  lookFSIG: ModuleId.modId -> 'a,
	  lookTYC: ModuleId.modId -> 'a,
	  lookEENV: ModuleId.modId -> 'a }

    datatype stubinfo =
	NoStub
      | SimpleStub
      | PrimStub
      | NodeStub of int * Symbol.symbol

    (* the environment pickler *)
    fun mkEnvPickler (context0: stubinfo context, isLocalPid) = let

	val { lookTYC, lookSIG, lookFSIG, lookSTR, lookFCT, lookEENV } =
	    context0

	val alphaConvert = mkAlphaConvert ()

	fun stamp (Stamps.STAMP { scope, count }) = let
	    val op $ = PU.$ ST
	in
	    case scope of
		Stamps.LOCAL => "A" $ [int (alphaConvert count)]
	      | Stamps.GLOBAL p =>
		    if isLocalPid p then "A" $ [int count]
		    else "B" $ [pid p, int count]
	      | Stamps.SPECIAL s => "C" $ [string s, int count]
	end

	val entVar = stamp
	val entPath = list entVar

	val op $ = PU.$ MI
	fun modId (MI.STRid { rlzn, sign }) = "1" $ [stamp rlzn, stamp sign]
	  | modId (MI.SIGid s) = "2" $ [stamp s]
	  | modId (MI.FCTid { rlzn, sign }) = "3" $ [stamp rlzn, modId sign]
	  | modId (MI.FSIGid { paramsig, bodysig }) =
	    "4" $ [stamp paramsig, stamp bodysig]
	  | modId (MI.TYCid a) = "5" $ [stamp a]
	  | modId (MI.EENVid s) = "6" $ [stamp s]

	val lvcount = ref 0
	val lvlist = ref []

	fun anotherLvar v = let
	    val j = !lvcount
	in
	    lvlist := v :: !lvlist;
	    lvcount := j + 1;
	    j
	end

	val { access, conrep } = mkAccess { lvar = int o anotherLvar,
					    isLocalPid = isLocalPid }

	val op $ = PU.$ SPATH
	fun spath (SP.SPATH p) = "s" $ [list symbol p]
	val op $ = PU.$ IPATH
	fun ipath (IP.IPATH p) = "i" $ [list symbol p]

	val label = symbol

	fun eqprop eqp = let
	    val op $ = PU.$ EQP
	    fun eqc T.YES = "\000"
	      | eqc T.NO = "\001"
	      | eqc T.IND = "\002"
	      | eqc T.OBJ = "\003"
	      | eqc T.DATA = "\004"
	      | eqc T.ABS = "\005"
	      | eqc T.UNDEF = "\006"
	in
	    eqc eqp $ []
	end

	fun datacon (T.DATACON { name, const, typ, rep, sign, lazyp }) = let
	    val op $ = PU.$ DATACON
	in
	    "c" $ [symbol name, bool const, ty typ, conrep rep,
		   consig sign, bool lazyp]
	end

	and tyckind arg = let
	    val op $ = PU.$ TYCKIND
	    fun tk (T.PRIMITIVE pt) = "a" $ [int (PT.pt_toint pt)]
	      | tk (T.DATATYPE { index, family, stamps, root,freetycs }) =
		"b" $ [int index, option entVar root,
		       dtypeInfo (stamps, family, freetycs)]
	      | tk (T.ABSTRACT tyc) = "c" $ [tycon tyc]
	      | tk (T.FLEXTYC tps) = "d" $ [] (* "f" $ tycpath tps *)
	      (*** I (Matthias) carried through this message from Zhong:
	       tycpath should never be pickled; the only way it can be
	       pickled is when pickling the domains of a mutually 
	       recursive datatypes; right now the mutually recursive
	       datatypes are not assigned accurate domains ... (ZHONG)
	       the preceding code is just a temporary gross hack. 
	       ***)
	      | tk T.FORMAL = "d" $ []
	      | tk T.TEMP = "e" $ []
	in
	    tk arg
	end

	and dtypeInfo x = let
	    val op $ = PU.$ DTI
	    fun dti_raw (ss, family, freetycs) =
		"a" $ [list stamp (Vector.foldr (op ::) [] ss),
		       dtFamily family, list tycon freetycs]
	in
	    share (DTs (Vector.sub (#1 x, 0))) dti_raw x
	end

	and dtFamily x = let
	    val op $ = PU.$ DTF
	    fun dtf_raw { mkey, members, lambdatyc } =
		"b" $ [stamp mkey,
		       list dtmember (Vector.foldr (op ::) [] members)]
	in
	    share (MBs (#mkey x)) dtf_raw x
	end

	and dtmember { tycname, dcons, arity, eq = ref e, lazyp, sign } = let
	    val op $ = PU.$ DTMEM
	in
	    "c" $ [symbol tycname, list nameRepDomain dcons, int arity,
		   eqprop e, bool lazyp, consig sign]
	end

	and nameRepDomain { name, rep, domain } = let
	    val op $ = PU.$ NRD
	in
	    "d" $ [symbol name, conrep rep, option ty domain]
	end

	and tycon arg = let
	    val op $ = PU.$ TYCON
	    fun tc (T.GENtyc x) =
		let val id = MI.TYCid (#stamp x)
		    fun gt_raw { stamp = s, arity, eq = ref eq, kind, path } =
			case lookTYC id of
			    SimpleStub => "A" $ [modId id]
			  | NoStub => "B" $ [stamp s, int arity, eqprop eq,
					     tyckind kind, ipath path]
			  | PrimStub => "I" $ [modId id]
			  | NodeStub (i, s) =>
				"J" $ [int i, symbol s, modId id]
		in
		    share (MIs (id, NONE)) gt_raw x
		end
	      | tc (T.DEFtyc x) = let
		    fun dt_raw x = let
			val { stamp = s, tyfun, strict, path } = x
			val T.TYFUN { arity, body } = tyfun
		    in
			"C" $ [stamp s, int arity, ty body,
			       list bool strict, ipath path]
		    end
		in
		    share (MIs (MI.TYCid (#stamp x), NONE)) dt_raw x
		end
	      | tc (T.PATHtyc { arity, entPath = ep, path }) =
		"D" $ [int arity, entPath ep, ipath path]
	      | tc (T.RECORDtyc l) = "E" $ [list label l]
	      | tc (T.RECtyc i) = "F" $ [int i]
	      | tc (T.FREEtyc i) = "G" $ [int i]
	      | tc T.ERRORtyc = "H" $ []
	in
	    tc arg
	end

	and ty arg = let
	    val op $ = PU.$ T
	    fun ty (T.VARty (ref (T.INSTANTIATED t))) = ty t
	      | ty (T.VARty (ref (T.OPEN _))) =
		bug "uninstantiated VARty in pickmod"
	      | ty (T.CONty (c, l)) = "a" $ [tycon c, list ty l]
	      | ty (T.IBOUND i) = "b" $ [int i]
	      | ty T.WILDCARDty = "c" $ []
	      | ty (T.POLYty { sign, tyfun = T.TYFUN { arity, body } }) =
		"d" $ [list bool sign, int arity, ty body]
	      | ty T.UNDEFty = "e" $ []
	      | ty _ = bug "unexpected type in pickmod-ty"
	in
	    ty arg
	end

	val op $ = PU.$ II
	fun inl_info (II.INL_PRIM (p, t)) = "A" $ [primop p, option ty t]
	  | inl_info (II.INL_STR sl) = "B" $ [list inl_info sl]
	  | inl_info II.INL_NO = "C" $ []
	  | inl_info _ = bug "unexpected inl_info in pickmod"

	val op $ = PU.$ VAR
	fun var (V.VALvar { access = a, info, path, typ = ref t }) =
	    "1" $ [access a, inl_info info, spath path, ty t]
	  | var (V.OVLDvar { name, options = ref p,
			     scheme = T.TYFUN { arity, body } }) =
	    "2" $ [symbol name, list overld p, int arity, ty body]
	  | var V.ERRORvar = "3" $ []

	and overld { indicator, variant } = let
	    val op $ = PU.$ OVERLD
	in
	    "o" $ [ty indicator, var variant]
	end

	fun fsigId (M.FSIG { paramsig = M.SIG { stamp = ps, ... },
			     bodysig = M.SIG { stamp = bs, ... },
			     ... }) =
	    MI.FSIGid { paramsig = ps, bodysig = bs }
	  | fsigId _ = bug "unexpected functor signature in fsigId"

	fun strDef arg = let
	    val op $ = PU.$ SD
	    fun sd (M.CONSTstrDef s) = "C" $ [Structure s]
	      | sd (M.VARstrDef (s, p)) = "V" $ [Signature s, entPath p]
	in
	    sd arg
	end

	(* 
	 * boundeps is not pickled right now, but it really should
	 * be pickled in the future.
	 *)
	and Signature arg = let
	    val op $ = PU.$ SG
	    fun sg  M.ERRORsig = "A" $ []
	      | sg (M.SIG x) = let
		    val id = MI.SIGid (#stamp x)
		    fun sig_raw x = let
			val { name, closed, fctflag, stamp = sta, symbols,
			      elements, boundeps = ref b, lambdaty = _,
			      typsharing, strsharing } = x
			val b = NONE		(* currently turned off *)
		    in
			case lookSIG id of
			    SimpleStub => "B" $ [modId id]
			  | NoStub =>
				"C" $ [option symbol name, bool closed,
				       bool fctflag, stamp sta,
				       list symbol symbols,
				       list (pair (symbol, spec)) elements,
				       option (list (pair (entPath, tkind))) b,
				       list (list spath) typsharing,
				       list (list spath) strsharing]
			  | PrimStub => "D" $ [modId id]
			  | NodeStub (i, s) =>
				"E" $ [int i, symbol s, modId id]
		    end
		in
		    share (MIs (id, NONE)) sig_raw x
		end
	in
	    sg arg
	end

	and fctSig arg = let
	    val op $ = PU.$ FSG
	    fun fsg M.ERRORfsig = "a" $ []
	      | fsg (fs as M.FSIG x) = let
		    val id = fsigId fs
		    fun fsig_raw x = let
			val { kind, paramsig, paramvar, paramsym, bodysig } = x
		    in
			case lookFSIG id of
			    SimpleStub => "b" $ [modId id]
			  | NoStub =>
				"c" $ [option symbol kind, Signature paramsig,
				       entVar paramvar,
				       option symbol paramsym,
				       Signature bodysig]
			  | PrimStub => "d" $ [modId id]
			  | NodeStub (i, s) =>
				"e" $ [int i, symbol s, modId id]
		    end
		in
		    share (MIs (id, NONE)) fsig_raw x
		end
	in
	    fsg arg
	end

	and spec arg = let
	    val op $ = PU.$ SP
	    fun sp (M.TYCspec { spec = t, entVar = v, repl, scope }) =
		"1" $ [tycon t, entVar v, bool repl, int scope]
	      | sp (M.STRspec { sign, slot, def, entVar = v }) =
		"2" $ [Signature sign, int slot,
		       option (pair (strDef, int)) def, entVar v]
	      | sp (M.FCTspec { sign, slot, entVar = v }) =
		"3" $ [fctSig sign, int slot, entVar v]
	      | sp (M.VALspec { spec = t, slot }) = "4" $ [ty t, int slot]
	      | sp (M.CONspec { spec = c, slot }) =
		"5" $ [datacon c, option int slot]
	in
	    sp arg
	end

	and entity arg = let
	    val op $ = PU.$ EN
	    fun en (M.TYCent t) = "A" $ [tycEntity t]
	      | en (M.STRent t) = "B" $ [strEntity t]
	      | en (M.FCTent t) = "C" $ [fctEntity t]
	      | en M.ERRORent = "D" $ []
	in
	    en arg
	end

	and fctClosure (M.CLOSURE { param, body, env }) = let
	    val op $ = PU.$ FCTC
	in
	    "f" $ [entVar param, strExp body, entityEnv env]
	end

	and Structure arg = let
	    val op $ = PU.$ STR
	    fun str (M.STRSIG { sign, entPath = p }) =
		"A" $ [Signature sign, entPath p]
	      | str M.ERRORstr = "B" $ []
	      | str (M.STR (x as { sign = M.SIG sign, ... })) = let
		    val id = MI.STRid { rlzn = #stamp (#rlzn x),
				        sign = #stamp sign }
		    fun s_raw { sign, rlzn, access = a, info } =
			case lookSTR id of
			    SimpleStub => "C" $ [modId id, access a]
			  | NoStub =>
				"D" $ [Signature sign, strEntity rlzn,
				       access a, inl_info info]
			  | PrimStub => "I" $ [modId id]
			  | NodeStub (i, s) =>
				 "J" $ [int i, symbol s, modId id, access a]
		in
		    share (MIs (id, acc_pid (#access x))) s_raw x
		end
	      | str _ = bug "unexpected structure in pickmod"
	in
	    str arg
	end

	and Functor arg = let
	    val op $ = PU.$ F
	    fun fct M.ERRORfct = "E" $ []
	      | fct (M.FCT x) = let
		    val id = MI.FCTid { rlzn = #stamp (#rlzn x),
				        sign = fsigId (#sign x) }
		    fun f_raw { sign, rlzn, access = a, info } =
			case lookFCT id of
			    SimpleStub => "F" $ [modId id, access a]
			  | NoStub =>
				"G" $ [fctSig sign, fctEntity rlzn,
				       access a, inl_info info]
			  | PrimStub => "H" $ [modId id]
			  | NodeStub (i, s) =>
				"I" $ [int i, symbol s, modId id, access a]
		in
		    share (MIs (id, acc_pid (#access x))) f_raw x
		end
	in
	    fct arg
	end

	and stampExp (M.CONST s) = PU.$ STE ("a", [stamp s])
	  | stampExp (M.GETSTAMP s) = PU.$ STE ("b", [strExp s])
	  | stampExp M.NEW = "c" $ []

        and tycExp (M.CONSTtyc t) = PU.$ TCE ("d", [tycon t])
	  | tycExp (M.FORMtyc t) = PU.$ TCE ("e", [tycon t])
	  | tycExp (M.VARtyc s) = PU.$ TCE ("f", [entPath s])

        and strExp arg = let
	    val op $ = PU.$ STRE
	    fun stre (M.VARstr s) = "g" $ [entPath s]
	      | stre (M.CONSTstr s) = "h" $ [strEntity s]
	      | stre (M.STRUCTURE { stamp = s, entDec }) =
		"i" $ [stampExp s, entityDec entDec]
	      | stre (M.APPLY (f, s)) = "j" $ [fctExp f, strExp s]
	      | stre (M.LETstr (e, s)) = "k" $ [entityDec e, strExp s]
	      | stre (M.ABSstr (s, e)) = "l" $ [Signature s, strExp e]
	      | stre (M.CONSTRAINstr { boundvar, raw, coercion }) =
		"m" $ [entVar boundvar, strExp raw, strExp coercion]
	      | stre (M.FORMstr fs) = "n" $ [fctSig fs]
	in
	    stre arg
	end

        and fctExp arg = let
	    val op $ = PU.$ FE
	    fun fe (M.VARfct s) = "o" $ [entPath s]
	      | fe (M.CONSTfct e) = "p" $ [fctEntity e]
	      | fe (M.LAMBDA { param, body }) =
		"q" $ [entVar param, strExp body]
	      | fe (M.LAMBDA_TP { param, body, sign }) =
		"r" $ [entVar param, strExp body, fctSig sign]
	      | fe (M.LETfct (e, f)) = "s" $ [entityDec e, fctExp f]
	in
	    fe arg
	end

        and entityExp arg = let
	    val op $ = PU.$ EE
	    fun ee (M.TYCexp t) = "t" $ [tycExp t]
	      | ee (M.STRexp s) = "u" $ [strExp s]
	      | ee (M.FCTexp f) = "v" $ [fctExp f]
	      | ee M.ERRORexp = "w" $ []
	      | ee M.DUMMYexp = "x" $ []
	in
	    ee arg
	end

        and entityDec arg = let
	    val op $ = PU.$ ED
	    fun ed (M.TYCdec (s, x)) = "A" $ [entVar s, tycExp x]
	      | ed (M.STRdec (s, x, n)) = "B" $ [entVar s, strExp x, symbol n]
	      | ed (M.FCTdec (s, x)) = "C" $ [entVar s, fctExp x]
	      | ed (M.SEQdec e) = "D" $ [list entityDec e]
	      | ed (M.LOCALdec (a, b)) = "E" $ [entityDec a, entityDec b]
	      | ed M.ERRORdec = "F" $ []
	      | ed M.EMPTYdec = "G" $ []
	in
	    ed arg
	end

        and entityEnv (M.MARKeenv (s, r)) =
 	    let val op $ = PU.$ EEV
		val id = MI.EENVid s
		fun mee_raw (s, r) =
		    case lookEENV id of
			SimpleStub => "D" $ [modId id]
		      | NoStub => "E" $ [stamp s, entityEnv r]
		      | PrimStub => "F" $ [modId id]
		      | NodeStub (i, s) => "G" $ [int i, symbol s, modId id]
	    in
		share (MIs (id, NONE)) mee_raw (s, r)
	    end
	  | entityEnv (M.BINDeenv (d, r)) =
	    PU.$ EEV ("A", [list (pair (entVar, entity)) (ED.listItemsi d),
		           entityEnv r])
	  | entityEnv M.NILeenv = "B" $ []
	  | entityEnv M.ERReenv = "C" $ []

        and strEntity { stamp = s, entities, lambdaty = _, rpath } = let
	    val op $ = PU.$ SEN
	in
	    "s" $ [stamp s, entityEnv entities, ipath rpath]
	end

        and fctEntity fe = let
	    val op $ = PU.$ FEN
	    val { stamp = s, closure, lambdaty = _, tycpath = _, rpath } = fe
	in
	    "f" $ [stamp s, fctClosure closure, ipath rpath]
	end

        and tycEntity x = tycon x

        fun fixity Fixity.NONfix = "N" $ []
	  | fixity (Fixity.INfix (i, j)) = PU.$ FX ("I", [int i, int j])

	val op $ = PU.$ B
	fun binding (B.VALbind x) = "1" $ [var x]
	  | binding (B.CONbind x) = "2" $ [datacon x]
	  | binding (B.TYCbind x) = "3" $ [tycon x]
	  | binding (B.SIGbind x) = "4" $ [Signature x]
	  | binding (B.STRbind x) = "5" $ [Structure x]
	  | binding (B.FSGbind x) = "6" $ [fctSig x]
	  | binding (B.FCTbind x) = "7" $ [Functor x]
	  | binding (B.FIXbind x) = "8" $ [fixity x]

	fun env e = let
	    val syms = ListMergeSort.uniqueSort symCmp (Env.symbols e)
	    val pairs = map (fn s => (s, Env.look (e, s))) syms
	in
	    list (pair (symbol, binding)) pairs
	end

	fun env'n'ctxt { env = e, ctxt } =
	    pair (env, list modId) (e, ModuleId.Set.listItems ctxt)
    in
	{ pickler = env, pickler' = env'n'ctxt,
	  exportLvarsGetter = fn () => rev (!lvlist) }
    end

    fun pickleEnv { context, env } = let
	fun cvt lk i =
	    case lk context i of
		SOME _ => SimpleStub
	      | NONE => NoStub

	val c = { lookSTR = cvt CMStaticEnv.lookSTR,
		  lookSIG = cvt CMStaticEnv.lookSIG,
		  lookFCT = cvt CMStaticEnv.lookFCT,
		  lookFSIG = cvt CMStaticEnv.lookFSIG,
		  lookTYC = cvt CMStaticEnv.lookTYC,
		  lookEENV = cvt CMStaticEnv.lookEENV }

	val { pickler, exportLvarsGetter, ... } =
	    mkEnvPickler (c, fn _ => false)
	val pickle = Byte.stringToBytes (PU.pickle emptyMap (pickler env))
	val exportLvars = exportLvarsGetter ()

	val hash = pickle2hash pickle

	val exportPid =
	    case exportLvars of
		[] => NONE
	      | _ => SOME hash
    in
	addPickles (Word8Vector.length pickle);
	{ hash = hash, pickle = pickle, exportLvars = exportLvars,
	  exportPid = exportPid }
    end

    fun repickleEnvHash { context, env, orig_hash } = let
	fun lk i =
	    if ModuleId.Set.member (context, i) then SimpleStub else NoStub
	val c = { lookSTR = lk, lookSIG = lk, lookFCT = lk,
		  lookFSIG = lk, lookTYC = lk, lookEENV = lk }
	fun isLocalPid p = PersStamps.compare (p, orig_hash) = EQUAL
	val { pickler, ... } = mkEnvPickler (c, isLocalPid)
	val pickle = Byte.stringToBytes (PU.pickle emptyMap (pickler env))
    in
	pickle2hash pickle
    end

    type env'n'ctxt = { env: StaticEnv.staticEnv, ctxt: ModuleId.Set.set }

    fun envPickler context = let
	val { lookSTR, lookSIG, lookFCT, lookFSIG, lookTYC, lookEENV } =
	    context
	fun cvt lk i =
	    case lk i of
		SOME PrimKey => PrimStub
	      | SOME (NodeKey (i, s)) => NodeStub (i, s)
	      | NONE => NoStub
	val c = { lookSTR = cvt lookSTR,
		  lookSIG = cvt lookSIG,
		  lookFCT = cvt lookFCT,
		  lookFSIG = cvt lookFSIG,
		  lookTYC = cvt lookTYC,
		  lookEENV = cvt lookEENV }
	val { pickler', ... } = mkEnvPickler (c, fn _ => false)
    in
	pickler'
    end

    (* the dummy environment pickler *)
    fun dontPickle (senv : StaticEnv.staticEnv, count) = let
	val hash = let
	    val toByte = Word8.fromLargeWord o Word32.toLargeWord
	    val >> = Word32.>>
	    infix >>
	    val w = Word32.fromInt count
	in
	    PS.fromBytes
	      (Word8Vector.fromList
	       [0w0,0w0,0w0,toByte(w >> 0w24),0w0,0w0,0w0,toByte(w >> 0w16),
		0w0,0w0,0w0,toByte(w >> 0w8),0w0,0w0,0w0,toByte(w)])
	end
        (* next line is an alternative to using Env.consolidate *)
	val syms = ListMergeSort.uniqueSort symCmp (Env.symbols senv)
	fun newAccess i = A.PATH (A.EXTERN hash, i)
	fun mapbinding (sym, (i, env, lvars)) =
	    case Env.look (senv, sym) of
		B.VALbind (V.VALvar {access=a, info=z, path=p, typ= ref t }) =>
		    (case a of
			 A.LVAR k => 
			     (i+1,
			      Env.bind (sym,
					B.VALbind (V.VALvar
						   { access = newAccess i,
						     info = z, path = p,
						     typ = ref t}),
					env),
			      k :: lvars)
		       | _ => bug ("dontPickle 1: " ^ A.prAcc a))
	      | B.STRbind (M.STR { sign = s, rlzn = r, access = a, info =z}) =>
		     (case a of
			  A.LVAR k => 
			      (i+1,
			       Env.bind (sym,
					 B.STRbind (M.STR
						    { access = newAccess i,
						      sign = s, rlzn = r,
						      info = z}),
					 env),
			       k :: lvars)
			| _ => bug ("dontPickle 2" ^ A.prAcc a))
	      | B.FCTbind (M.FCT { sign = s, rlzn = r, access = a, info=z }) =>
		      (case a of
			   A.LVAR k => 
			       (i+1,
				Env.bind (sym,
					  B.FCTbind (M.FCT
						     { access = newAccess i,
						       sign = s, rlzn = r,
						       info = z}),
					  env),
				k :: lvars)
			 | _ => bug ("dontPickle 3" ^ A.prAcc a))
	      | B.CONbind (T.DATACON { name = n, const = c, typ = t, sign = s,
				       lazyp= false, rep as (A.EXN a) }) => let
		    val newrep = A.EXN (newAccess i)
		in
		    case a of
			A.LVAR k =>
			    (i+1,
			     Env.bind (sym,
				       B.CONbind (T.DATACON
						  { rep = newrep, name = n,
						    lazyp = false,
						    const = c, typ = t,
						    sign = s }),
				       env),
			     k :: lvars)
		      | _ => bug ("dontPickle 4" ^ A.prAcc a)
		end
	      | binding =>  (i, Env.bind (sym, binding, env), lvars)
	val (_,newenv,lvars) = foldl mapbinding (0, StaticEnv.empty, nil) syms
	val exportPid =
	    case lvars of
		[] => NONE
	      | _ => SOME hash
    in
	(newenv, hash, rev lvars, exportPid)
    end
  end
end


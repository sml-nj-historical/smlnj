(*
 * The new unpickler (based on the new generic unpickling facility).
 *
 * July 1999, Matthias Blume
 *)
signature UNPICKMOD = sig

    type env'n'ctxt = { env: StaticEnv.staticEnv, ctxt: ModuleId.Set.set }

    val unpickleEnv :
	{ context: CMStaticEnv.staticEnv,
	  hash: PersStamps.persstamp,
	  pickle: Word8Vector.vector }
	-> env'n'ctxt

    val unpickleFLINT : Word8Vector.vector -> CompBasic.flint option

    (*
     * The env unpickler resulting from "mkUnpicklers" cannot be used for
     * "original" environments that come out of the elaborator.  For those,
     * continue to use "unpickleEnv".  "mkUnpicklers" is intended to be
     * used by CM's stable library mechanism.
     *)
    val mkUnpicklers :
	UnpickleUtil.session ->
	{ prim_context: string -> CMStaticEnv.staticEnv option,
	  node_context: int * Symbol.symbol -> CMStaticEnv.staticEnv option }
	-> { symenv: SymbolicEnv.symenv UnpickleUtil.reader,
	     env: env'n'ctxt UnpickleUtil.reader,
	     symbol: Symbol.symbol UnpickleUtil.reader,
	     symbollist: Symbol.symbol list UnpickleUtil.reader }
end

structure UnpickMod : UNPICKMOD = struct

    structure A = Access
    structure DI = DebIndex
    structure LT = LtyDef
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

    structure UU = UnpickleUtil
    exception Format = UU.Format

    type env'n'ctxt = { env: StaticEnv.staticEnv, ctxt: ModuleId.Set.set }

    (* The order of the entries in the following tables
     * must be coordinated with pickmod! *)
    val primop_table =
	#[P.MKETAG,
	  P.WRAP,
	  P.UNWRAP,
	  P.SUBSCRIPT,
	  P.SUBSCRIPTV,
	  P.INLSUBSCRIPT,
	  P.INLSUBSCRIPTV,
	  P.INLMKARRAY,

	  P.PTREQL,
	  P.PTRNEQ,
	  P.POLYEQL,
	  P.POLYNEQ,
	  P.BOXED,
	  P.UNBOXED,
	  P.LENGTH,
	  P.OBJLENGTH,
	  P.CAST,
	  P.GETRUNVEC,
	  P.MARKEXN,
	  P.GETHDLR,
	  P.SETHDLR,
	  P.GETVAR,
	  P.SETVAR,
	  P.GETPSEUDO,
	  P.SETPSEUDO,
	  P.SETMARK,
	  P.DISPOSE,
	  P.MAKEREF,
	  P.CALLCC,
	  P.CAPTURE,
	  P.THROW,
	  P.DEREF,
	  P.ASSIGN,
	  P.UPDATE,
	  P.INLUPDATE,
	  P.BOXEDUPDATE,
	  P.UNBOXEDUPDATE,

	  P.GETTAG,
	  P.MKSPECIAL,
	  P.SETSPECIAL,
	  P.GETSPECIAL,
	  P.USELVAR,
	  P.DEFLVAR,
	  P.INLDIV,
	  P.INLMOD,
	  P.INLREM,
	  P.INLMIN,
	  P.INLMAX,
	  P.INLABS,
	  P.INLNOT,
	  P.INLCOMPOSE,
	  P.INLBEFORE,
	  P.INL_ARRAY,
	  P.INL_VECTOR,
	  P.ISOLATE,
	  P.WCAST,
	  P.NEW_ARRAY0,
	  P.GET_SEQ_DATA,
	  P.SUBSCRIPT_REC,
	  P.SUBSCRIPT_RAW64,
	  P.UNBOXEDASSIGN]

    val cmpop_table =
	#[P.>, P.>=, P.<, P.<=, P.LEU, P.LTU, P.GEU, P.GTU, P.EQL, P.NEQ]

    val arithop_table =
	#[P.+, P.-, P.*, P./, P.~, P.ABS, P.LSHIFT, P.RSHIFT, P.RSHIFTL,
	  P.ANDB, P.ORB, P.XORB, P.NOTB]

    val eqprop_table =
	#[T.YES, T.NO, T.IND, T.OBJ, T.DATA, T.ABS, T.UNDEF]

    fun mkSharedStuff (session, lvar) = let

	fun share m f = UU.share session m f
	fun nonshare f = UU.nonshare session f

	val int = UU.r_int session
	val bool = UU.r_bool session
	fun list m r = UU.r_list session m r
	val string = UU.r_string session
	val symbol = UnpickleSymPid.r_symbol (session, string)

	(* These maps will all acquire different types by being used in
	 * different contexts... *)
	val accM = UU.mkMap ()
	val crM = UU.mkMap ()
	val csM = UU.mkMap ()
	val ltyM = UU.mkMap ()
	val ltyListM = UU.mkMap ()
	val tycM = UU.mkMap ()
	val tycListM = UU.mkMap ()
	val tkindM = UU.mkMap ()
	val tkindListM = UU.mkMap ()
	val nkM = UU.mkMap ()
	val poM = UU.mkMap ()
	val boolListM = UU.mkMap ()

	val boollist = list boolListM bool

	val pid = UnpickleSymPid.r_pid string
	    
	fun access () = let
	    fun a #"A" = lvar (int ())
	      | a #"B" = A.EXTERN (pid ())
	      | a #"C" = A.PATH (access (), int ())
	      | a #"D" = A.NO_ACCESS
	      | a _ = raise Format
	in
	    share accM a
	end

	fun conrep () = let
	    fun cr #"A" = A.UNTAGGED
	      | cr #"B" = A.TAGGED (int ())
	      | cr #"C" = A.TRANSPARENT
	      | cr #"D" = A.CONSTANT (int ())
	      | cr #"E" = A.REF
	      | cr #"F" = A.EXN (access ())
	      | cr #"G" = A.LISTCONS
	      | cr #"H" = A.LISTNIL
	      | cr #"I" = A.SUSP NONE
	      | cr #"J" = A.SUSP (SOME (access (), access ()))
	      | cr _ = raise Format
	in
	    share crM cr
	end

	fun consig () = let
	    fun cs #"S" = A.CSIG (int (), int ())
	      | cs #"N" = A.CNIL
	      | cs _ = raise Format
	in
	    share csM cs
	end

	fun lty () = let
	    fun lt #"A" = LT.ltc_tyc (tyc ())
	      | lt #"B" = LT.ltc_str (ltylist ())
	      | lt #"C" = LT.ltc_fct (ltylist (), ltylist ())
	      | lt #"D" = LT.ltc_poly (tkindlist (), ltylist ())
	      | lt _ = raise Format
	in
	    share ltyM lt
	end

	and ltylist () = list ltyListM lty ()

	and tyc () = let
	    fun tc #"A" = LT.tcc_var (DI.di_fromint (int ()), int ())
	      | tc #"B" = LT.tcc_nvar (int (), DI.di_fromint (int ()), int ())
	      | tc #"C" = LT.tcc_prim (PT.pt_fromint (int ()))
	      | tc #"D" = LT.tcc_fn (tkindlist (), tyc ())
	      | tc #"E" = LT.tcc_app (tyc (), tyclist ())
	      | tc #"F" = LT.tcc_seq (tyclist ())
	      | tc #"G" = LT.tcc_proj (tyc (), int ())
	      | tc #"H" = LT.tcc_sum (tyclist ())
	      | tc #"I" = LT.tcc_fix ((int (), tyc (), tyclist ()), int ())
	      | tc #"J" = LT.tcc_abs (tyc ())
	      | tc #"K" = LT.tcc_box (tyc ())
	      | tc #"L" = LT.tcc_tuple (tyclist ())
	      | tc #"M" = LT.tcc_arrow (LT.ffc_var (bool (), bool ()),
					tyclist (), tyclist ())
	      | tc #"N" = LT.tcc_arrow (LT.ffc_fixed, tyclist (), tyclist ())
	      | tc #"O" = LK.tc_inj (LK.TC_TOKEN (LK.token_key (int ()),
						  tyc ()))
	      | tc _ = raise Format
	in
	    share tycM tc
	end

	and tyclist () = list tycListM tyc ()

	and tkind () = let
	    fun tk #"A" = LT.tkc_mono
	      | tk #"B" = LT.tkc_box
	      | tk #"C" = LT.tkc_seq (tkindlist ())
	      | tk #"D" = LT.tkc_fun (tkindlist (), tkind ())
	      | tk _ = raise Format
	in
	    share tkindM tk
	end

	and tkindlist () = list tkindListM tkind ()

	fun numkind () = let
	    fun nk #"A" = P.INT (int ())
	      | nk #"B" = P.UINT (int ())
	      | nk #"C" = P.FLOAT (int ())
	      | nk _ = raise Format
	in
	    share nkM nk
	end

	fun arithop () = let
	    fun ao c =
		Vector.sub (arithop_table, Char.ord c)
		handle General.Subscript => raise Format
	in
	    nonshare ao
	end

	fun cmpop () = let
	    fun co c =
		Vector.sub (cmpop_table, Char.ord c)
		handle General.Subscript => raise Format
	in
	    nonshare co
	end

	fun primop () = let
	    fun po #"\100" = P.ARITH { oper = arithop (), overflow = bool (),
				       kind = numkind () }
	      | po #"\101" = P.CMP { oper = cmpop (), kind = numkind () }
	      | po #"\102" = P.TEST (int (), int ())
	      | po #"\103" = P.TESTU (int (), int ())
	      | po #"\104" = P.TRUNC (int (), int ())
	      | po #"\105" = P.EXTEND (int (), int ())
	      | po #"\106" = P.COPY (int (), int ())
	      | po #"\107" = P.INLLSHIFT (numkind ())
	      | po #"\108" = P.INLRSHIFT (numkind ())
	      | po #"\109" = P.INLRSHIFTL (numkind ())
	      | po #"\110" = P.ROUND { floor = bool (), fromkind = numkind (),
				       tokind = numkind () }
	      | po #"\111" = P.REAL { fromkind = numkind (),
				      tokind = numkind ()}
	      | po #"\112" = P.NUMSUBSCRIPT { kind = numkind (),
					      checked = bool (),
					      immutable = bool () }
	      | po #"\113" = P.NUMUPDATE { kind = numkind (),
					   checked = bool () }
	      | po #"\114" = P.INL_MONOARRAY (numkind ())
	      | po #"\115" = P.INL_MONOVECTOR (numkind ())
	      | po c =
		Vector.sub (primop_table, Char.ord c)
		handle General.Subscript => raise Format
	in
	    share poM po
	end
    in
	{ pid = pid, string = string, symbol = symbol,
	  access = access, conrep = conrep, consig = consig,
	  lty = lty, tyc = tyc, tkind = tkind,
	  ltylist = ltylist, tyclist = tyclist,
	  primop = primop, boollist = boollist }
    end

    fun mkEnvUnpickler arg = let
	val (session, symbollist, sharedStuff, context0, globalPid) = arg

	val { lookTYC, lookSIG, lookFSIG, lookSTR, lookFCT, lookEENV,
	      lookTYCp, lookSIGp, lookFSIGp, lookSTRp, lookFCTp, lookEENVp,
	      lookTYCn, lookSIGn, lookFSIGn, lookSTRn, lookFCTn, lookEENVn } =
	    context0

	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r
	val bool = UU.r_bool session
	val pair = UU.r_pair
	val int = UU.r_int session

	fun share m f = UU.share session m f
	fun nonshare f = UU.nonshare session f

	(* The following maps all acquire different types by being used
	 * in different contexts: *)
	val stampM = UU.mkMap ()
	val stampOptionM = UU.mkMap ()
	val stampListM = UU.mkMap ()
	val modIdM = UU.mkMap ()
	val symbolOptionM = UU.mkMap ()
	val symbolListM = UU.mkMap ()
	val spathListM = UU.mkMap ()
	val spathListListM = UU.mkMap ()
	val dataconM = UU.mkMap ()
	val tkM = UU.mkMap ()
	val dtiM = UU.mkMap ()
	val dtfM = UU.mkMap ()
	val dtmemberM = UU.mkMap ()
	val dtmListM = UU.mkMap ()
	val nrdM = UU.mkMap ()
	val nrdListM = UU.mkMap ()
	val tyconM = UU.mkMap ()
	val tyconListM = UU.mkMap ()
	val tyM = UU.mkMap ()
	val tyOptionM = UU.mkMap ()
	val tyListM = UU.mkMap ()
	val iiM = UU.mkMap ()
	val vM = UU.mkMap ()
	val sdM = UU.mkMap ()
	val sigM = UU.mkMap ()
	val fsigM = UU.mkMap ()
	val spM = UU.mkMap ()
	val enM = UU.mkMap ()
	val fctcM = UU.mkMap ()
	val strM = UU.mkMap ()
	val fctM = UU.mkMap ()
	val steM = UU.mkMap ()
	val tceM = UU.mkMap ()
	val streM = UU.mkMap ()
	val feM = UU.mkMap ()
	val eeM = UU.mkMap ()
	val edM = UU.mkMap ()
	val eenvM = UU.mkMap ()
	val senM = UU.mkMap ()
	val fenM = UU.mkMap ()
	val fxM = UU.mkMap ()
	val bM = UU.mkMap ()
	val elementsM = UU.mkMap ()
	val bepsLM = UU.mkMap ()
	val bepsOM = UU.mkMap ()
	val spDefM = UU.mkMap ()
	val iiListM = UU.mkMap ()
	val overldM = UU.mkMap ()
	val olListM = UU.mkMap ()
	val ioM = UU.mkMap ()
	val edListM = UU.mkMap ()
	val eenvBindM = UU.mkMap ()
	val envM = UU.mkMap ()
	val milM = UU.mkMap ()

	val { pid, string, symbol,
	      access, conrep, consig, lty, tyc, tkind, ltylist, tyclist,
	      primop, boollist } = sharedStuff

	fun stamp () = let
	    fun st #"A" = Stamps.STAMP { scope = Stamps.GLOBAL (globalPid ()),
					 count = int () }
	      | st #"B" = Stamps.STAMP { scope = Stamps.GLOBAL (pid ()),
					 count = int () }
	      | st #"C" = Stamps.STAMP { scope = Stamps.SPECIAL (string ()),
					 count = int () }
	      | st _ = raise Format
	in
	    share stampM st
	end

	val stamplist = list stampListM stamp
	val stampoption = option stampOptionM stamp

	val entVar = stamp
	val entVarOption = stampoption
	val entPath = stamplist

	fun modId () = let
	    fun mi #"1" = MI.STRid { rlzn = stamp (), sign = stamp () }
	      | mi #"2" = MI.SIGid (stamp ())
	      | mi #"3" = MI.FCTid { rlzn = stamp (), sign = modId () }
	      | mi #"4" = MI.FSIGid { paramsig = stamp (),
				      bodysig  = stamp () }
	      | mi #"5" = MI.TYCid (stamp ())
	      | mi #"6" = MI.EENVid (stamp ())
	      | mi _ = raise Format
	in
	    share modIdM mi
	end

	val symbollist = list symbolListM symbol
	val symboloption = option symbolOptionM symbol

	fun spath () = SP.SPATH (symbollist ())
	fun ipath () = IP.IPATH (symbollist ())

	val spathlist = list spathListM spath
	val spathlistlist = list spathListListM spathlist

	val label = symbol
	val labellist = symbollist

	fun eqprop () = let
	    fun eqp c =
		Vector.sub (eqprop_table, Char.ord c)
		handle General.Subscript => raise Format
	in
	    nonshare eqp
	end

	fun datacon () = let
	    fun d #"c" =
		T.DATACON { name = symbol (), const = bool (), typ = ty (),
			    rep = conrep (), sign = consig (),
			    lazyp = bool () }
	      | d _ = raise Format
	in
	    share dataconM d
	end

	and tyckind () = let
	    fun tk #"a" = T.PRIMITIVE (PT.pt_fromint (int ()))
	      | tk #"b" = let
		    val index = int ()
		    val root = entVarOption ()
		    val (stamps, family, freetycs) = dtypeInfo ()
		in
		    T.DATATYPE { index = index, root = root,
				 stamps = stamps, family = family,
				 freetycs = freetycs }
		end
	      | tk #"c" = T.ABSTRACT (tycon ())
	      | tk #"d" = T.FORMAL
	      | tk #"e" = T.TEMP
	      | tk _ = raise Format
	in
	    share tkM tk
	end

	and dtypeInfo () = let
	    fun dti #"a" =
		(Vector.fromList (stamplist ()), dtFamily (), tyconlist ())
	      | dti _ = raise Format
	in
	    share dtiM dti
	end

	and dtFamily () = let
	    fun dtf #"b" =
		{ mkey = stamp (),
		  members = Vector.fromList (dtmemberlist ()),
		  lambdatyc = ref NONE }
	      | dtf _ = raise Format
	in
	    share dtfM dtf
	end

	and dtmember () = let
	    fun d #"c" = { tycname = symbol (), dcons = nrdlist (),
			   arity = int (), eq = ref (eqprop ()),
			   lazyp = bool (), sign = consig () }
	      | d _ = raise Format
	in
	    share dtmemberM d
	end

	and dtmemberlist () = list dtmListM dtmember ()

	and nameRepDomain () = let
	    fun n #"d" =
		{ name = symbol (), rep = conrep (), domain = tyoption () }
	      | n _ = raise Format
	in
	    share nrdM n
	end

	and nrdlist () = list nrdListM nameRepDomain ()

	and tycon () = let
	    fun tyc #"A" = lookTYC (modId ())
	      | tyc #"B" = T.GENtyc { stamp = stamp (), arity = int (),
				      eq = ref (eqprop ()), kind = tyckind (),
				      path = ipath () }
	      | tyc #"C" = T.DEFtyc { stamp = stamp (),
				      tyfun = T.TYFUN { arity = int (),
						        body = ty () },
				      strict = boollist (),
				      path = ipath () }
	      | tyc #"D" =  T.PATHtyc { arity = int (), entPath = entPath (),
				        path = ipath () }
	      | tyc #"E" = T.RECORDtyc (labellist ())
	      | tyc #"F" = T.RECtyc (int ())
	      | tyc #"G" = T.FREEtyc (int ())
	      | tyc #"H" = T.ERRORtyc
	      | tyc #"I" = lookTYCp (string (), modId ())
	      | tyc #"J" = lookTYCn (int (), symbol(), modId ())
	      | tyc _ = raise Format
	in
	    share tyconM tyc
	end

	and tyconlist () = list tyconListM tycon ()

	and ty () = let
	    fun t #"a" = T.CONty (tycon (), tylist ())
	      | t #"b" = T.IBOUND (int ())
	      | t #"c" = T.WILDCARDty
	      | t #"d" = T.POLYty { sign = boollist (),
				    tyfun = T.TYFUN { arity = int (),
					 	      body = ty () } }
	      | t #"e" = T.UNDEFty
	      | t _ = raise Format
	in
	    share tyM t
	end

	and tyoption () = option tyOptionM ty ()
	and tylist () = list tyListM ty ()

	and inl_info () = let
	    fun ii #"A" = II.INL_PRIM (primop (), tyoption ())
	      | ii #"B" = II.INL_STR (iilist ())
	      | ii #"C" = II.INL_NO
	      | ii _ = raise Format
	in
	    share iiM ii
	end

	and iilist () = list iiListM inl_info ()

	and var () = let
	    fun v #"1" = V.VALvar { access = access (), info = inl_info (),
				    path = spath (), typ = ref (ty ()) }
	      | v #"2" = V.OVLDvar { name = symbol (),
				     options = ref (overldlist ()),
				     scheme = T.TYFUN { arity = int (),
						        body = ty () } }
	      | v #"3" = V.ERRORvar
	      | v _ = raise Format
	in
	    share vM v
	end

	and overld () = let
	    fun ov #"o" = { indicator = ty (), variant = var () }
	      | ov _ = raise Format
	in
	    share overldM ov
	end

	and overldlist () = list olListM overld ()

	fun strDef () = let
	    fun sd #"C" = M.CONSTstrDef (Structure ())
	      | sd #"V" = M.VARstrDef (Signature (), entPath ())
	      | sd _ = raise Format
	in
	    share sdM sd
	end

	and Signature () = let
	    fun sg #"A" = M.ERRORsig
	      | sg #"B" = lookSIG (modId ())
	      | sg #"C" = M.SIG { name = symboloption (),
				  closed = bool (),
				  fctflag = bool (),
				  stamp = stamp (),
				  symbols = symbollist (),
				  elements = list elementsM
				                 (pair (symbol, spec)) (),
				  boundeps =
				    ref (option bepsOM
					 (list bepsLM (pair (entPath,
							     tkind))) ()),
				  lambdaty = ref NONE,
				  typsharing = spathlistlist (),
				  strsharing = spathlistlist () }
	      | sg #"D" = lookSIGp (string (), modId ())
	      | sg #"E" = lookSIGn (int (), symbol (), modId ())
	      | sg _ = raise Format
	in
	    share sigM sg
	end

	and fctSig () = let
	    fun fsg #"a" = M.ERRORfsig
	      | fsg #"b" = lookFSIG (modId ())
	      | fsg #"c" = M.FSIG { kind = symboloption (),
				    paramsig = Signature (),
				    paramvar = entVar (),
				    paramsym = symboloption (),
				    bodysig = Signature () }
	      | fsg #"d" = lookFSIGp (string (), modId ())
	      | fsg #"e" = lookFSIGn (int (), symbol (), modId ())
	      | fsg _ = raise Format
	in
	    share fsigM fsg
	end

	and spec () = let
	    val intoption = option ioM int
	    fun sp #"1" = M.TYCspec { spec = tycon (), entVar = entVar (),
				      repl = bool (), scope = int () }
	      | sp #"2" = M.STRspec { sign = Signature (), slot = int (),
				      def = option spDefM
				               (pair (strDef, int)) (),
				      entVar = entVar () }
	      | sp #"3" = M.FCTspec { sign = fctSig (), slot = int (),
				      entVar = entVar () }
	      | sp #"4" = M.VALspec { spec = ty (), slot = int () }
	      | sp #"5" = M.CONspec { spec = datacon (), slot = intoption () }
	      | sp _ = raise Format
	in
	    share spM sp
	end

	and entity () = let
	    fun en #"A" = M.TYCent (tycEntity ())
	      | en #"B" = M.STRent (strEntity ())
	      | en #"C" = M.FCTent (fctEntity ())
	      | en #"D" = M.ERRORent
	      | en _ = raise Format
	in
	    share enM en
	end

	and fctClosure () = let
	    fun f #"f" =M.CLOSURE { param = entVar (), body = strExp (),
				    env = entityEnv () }
	      | f _ = raise Format
	in
	    share fctcM f
	end

	and Structure () = let
	    fun stracc (M.STR { sign, rlzn, info, ... }) =
		M.STR { sign = sign, rlzn = rlzn, info = info,
		        access = access () }
	      | stracc _ = raise Format
	    fun str #"A" = M.STRSIG { sign = Signature (),
				      entPath = entPath () }
	      | str #"B" = M.ERRORstr
	      | str #"C" = stracc (lookSTR (modId ()))
	      | str #"D" = M.STR { sign = Signature (), rlzn = strEntity (),
				   access = access (), info = inl_info () }
	      | str #"I" = stracc (lookSTRp (string (), modId ()))
	      | str #"J" = stracc (lookSTRn (int (), symbol (), modId ()))
	      | str _ = raise Format
	in
	    share strM str
	end

	and Functor () = let
	    fun fctacc (M.FCT { sign, rlzn, info, ... }) =
		M.FCT { sign = sign, rlzn = rlzn, info = info,
		        access = access () }
	      | fctacc _ = raise Format
	    fun fct #"E" = M.ERRORfct
	      | fct #"F" = fctacc (lookFCT (modId ()))
	      | fct #"G" = M.FCT { sign = fctSig (), rlzn = fctEntity (),
				   access = access (), info = inl_info () }
	      | fct #"H" = fctacc (lookFCTp (string (), modId ()))
	      | fct #"I" = fctacc (lookFCTn (int (), symbol (), modId ()))
	      | fct _ = raise Format
	in
	    share fctM fct
	end

	and stampExp () = let
	    fun ste #"a" = M.CONST (stamp ())
	      | ste #"b" = M.GETSTAMP (strExp ())
	      | ste #"c" = M.NEW
	      | ste _ = raise Format
	in
	    share steM ste
	end

	and tycExp () = let
	    fun tce #"d" = M.CONSTtyc (tycon ())
	      | tce #"e" = M.FORMtyc (tycon ())
	      | tce #"f" = M.VARtyc (entPath ())
	      | tce _ = raise Format
	in
	    share tceM tce
	end

	and strExp () = let
	    fun stre #"g" = M.VARstr (entPath ())
	      | stre #"h" = M.CONSTstr (strEntity ())
	      | stre #"i" = M.STRUCTURE { stamp = stampExp (),
					  entDec = entityDec () }
	      | stre #"j" = M.APPLY (fctExp (), strExp ())
	      | stre #"k" = M.LETstr (entityDec (), strExp ())
	      | stre #"l" = M.ABSstr (Signature (), strExp ())
	      | stre #"m" = M.CONSTRAINstr { boundvar = entVar (),
					     raw = strExp (),
					     coercion = strExp () }
	      | stre #"n" = M.FORMstr (fctSig ())
	      | stre _ = raise Format
	in
	    share streM stre
	end

	and fctExp () = let
	    fun fe #"o" = M.VARfct (entPath ())
	      | fe #"p" = M.CONSTfct (fctEntity ())
	      | fe #"q" = M.LAMBDA { param = entVar (), body = strExp () }
	      | fe #"r" = M.LAMBDA_TP { param = entVar (), body = strExp (),
				        sign = fctSig () }
	      | fe #"s" = M.LETfct (entityDec (), fctExp ())
	      | fe _ = raise Format
	in
	    share feM fe
	end

	and entityExp () = let
	    fun ee #"t" = M.TYCexp (tycExp ())
	      | ee #"u" = M.STRexp (strExp ())
	      | ee #"v" = M.FCTexp (fctExp ())
	      | ee #"w" = M.ERRORexp
	      | ee #"x" = M.DUMMYexp
	      | ee _ = raise Format
	in
	    share eeM ee
	end

	and entityDec () = let
	    fun ed #"A" = M.TYCdec (entVar (), tycExp ())
	      | ed #"B" = M.STRdec (entVar (), strExp (), symbol ())
	      | ed #"C" = M.FCTdec (entVar (), fctExp ())
	      | ed #"D" = M.SEQdec (entityDecList ())
	      | ed #"E" = M.LOCALdec (entityDec (), entityDec ())
	      | ed #"F" = M.ERRORdec
	      | ed #"G" = M.EMPTYdec
	      | ed _ = raise Format
	in
	    share edM ed
	end

	and entityDecList () = list edListM entityDec ()

	and entityEnv () = let
	    fun eenv #"A" =
		let
		    val l = list eenvBindM (pair (entVar, entity)) ()
		    fun add ((v, e), z) = ED.insert (z, v, e)
		    val ed = foldr add ED.empty l
		in
		    M.BINDeenv (ed, entityEnv ())
		end
	      | eenv #"B" = M.NILeenv
	      | eenv #"C" = M.ERReenv
	      | eenv #"D" = lookEENV (modId ())
	      | eenv #"E" = M.MARKeenv (stamp (), entityEnv ())
	      | eenv #"F" = lookEENVp (string (), modId ())
	      | eenv #"G" = lookEENVn (int (), symbol (), modId ())
	      | eenv _ = raise Format
	in
	    share eenvM eenv
	end

	and strEntity () = let
	    fun s #"s" =
		{ stamp = stamp (), entities = entityEnv (), rpath = ipath (),
		  lambdaty = ref NONE }
	      | s _ = raise Format
	in
	    share senM s
	end

	and fctEntity () = let
	    fun f #"f" =
		{ stamp = stamp (), closure = fctClosure (), rpath = ipath (),
		  lambdaty = ref NONE, tycpath = NONE }
	      | f _ = raise Format
	in
	    share fenM f
	end

	and tycEntity () = tycon ()

	fun fixity () = let
	    fun fx #"N" = Fixity.NONfix
	      | fx #"I" = Fixity.INfix (int (), int ())
	      | fx _ = raise Format
	in
	    share fxM fx
	end

	fun binding () = let
	    fun b #"1" = B.VALbind (var ())
	      | b #"2" = B.CONbind (datacon ())
	      | b #"3" = B.TYCbind (tycon ())
	      | b #"4" = B.SIGbind (Signature ())
	      | b #"5" = B.STRbind (Structure ())
	      | b #"6" = B.FSGbind (fctSig ())
	      | b #"7" = B.FCTbind (Functor ())
	      | b #"8" = B.FIXbind (fixity ())
	      | b _ = raise Format
	in
	    share bM b
	end

	fun env () = let
	    val bindlist = list envM (pair (symbol, binding)) ()
	    fun bind ((s, b), e) = Env.bind (s, b, e)
	in
	    Env.consolidate (foldl bind Env.empty bindlist)
	end

	fun env' () = let
	    val (e, mil) = pair (env, list milM modId) ()
	    val ctxt = ModuleId.Set.addList (ModuleId.Set.empty, mil)
	in
	    { env = e, ctxt = ctxt }
	end
    in
	{ envUnpickler = env, envUnpickler' = env' }
    end

    fun unpickleEnv { context, hash, pickle } = let
	val cs = ref ModuleId.Set.empty
	fun cvt lk i =
	    case lk context i of
		SOME v => (cs := ModuleId.Set.add (!cs, i); v)
	      | NONE => raise Format
	fun dont _ = raise Format
	val c = { lookSTR = cvt CMStaticEnv.lookSTR,
		  lookSIG = cvt CMStaticEnv.lookSIG,
		  lookFCT = cvt CMStaticEnv.lookFCT,
		  lookFSIG = cvt CMStaticEnv.lookFSIG,
		  lookTYC = cvt CMStaticEnv.lookTYC,
		  lookEENV = cvt CMStaticEnv.lookEENV,
		  lookSTRp = dont,
		  lookSIGp = dont,
		  lookFCTp = dont,
		  lookFSIGp = dont,
		  lookTYCp = dont,
		  lookEENVp = dont,
		  lookSTRn = dont,
		  lookSIGn = dont,
		  lookFCTn = dont,
		  lookFSIGn = dont,
		  lookTYCn = dont,
		  lookEENVn = dont }
	val session =
	    UU.mkSession (UU.stringGetter (Byte.bytesToString pickle))
	fun import i = A.PATH (A.EXTERN hash, i)
	val sharedStuff as { symbol, ... } = mkSharedStuff (session, import)
	val symbolListM = UU.mkMap ()
	val symbollist = UU.r_list session symbolListM symbol
	val { envUnpickler, ... } =
	    mkEnvUnpickler (session, symbollist, sharedStuff,
			    c, fn () => hash)
    in
	(* order of evaluation is important here! *)
	{ env = envUnpickler (), ctxt = !cs }
    end

    fun mkFlintUnpickler (session, sharedStuff) = let

	fun share m r = UU.share session m r

	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r

	val pair = UU.r_pair
	val int = UU.r_int session
	val int32 = UU.r_int32 session
	val word = UU.r_word session
	val word32 = UU.r_word32 session
	val bool = UU.r_bool session

	val { pid, string, symbol,
	      access, conrep, consig, lty, tyc, tkind, ltylist, tyclist,
	      primop, boollist } = sharedStuff

	val valueM = UU.mkMap ()
	val conM = UU.mkMap ()
	val dconM = UU.mkMap ()
	val dictM = UU.mkMap ()
	val fprimM = UU.mkMap ()
	val lexpM = UU.mkMap ()
	val fkindM = UU.mkMap ()
	val rkindM = UU.mkMap ()
	val ltyloM = UU.mkMap ()
	val dictTableM = UU.mkMap ()
	val dictOptionM = UU.mkMap ()
	val valueListM = UU.mkMap ()
	val lvarListM = UU.mkMap ()
	val fundecListM = UU.mkMap ()
	val conListM = UU.mkMap ()
	val lexpOptionM = UU.mkMap ()
	val fundecM = UU.mkMap ()
	val tfundecM = UU.mkMap ()
	val fdplM = UU.mkMap ()
	val tfplM = UU.mkMap ()

	val lvar = int
	val lvarlist = list lvarListM lvar

	fun value () = let
	    fun v #"a" = F.VAR (lvar ())
	      | v #"b" = F.INT (int ())
	      | v #"c" = F.INT32 (int32 ())
	      | v #"d" = F.WORD (word ())
	      | v #"e" = F.WORD32 (word32 ())
	      | v #"f" = F.REAL (string ())
	      | v #"g" = F.STRING (string ())
	      | v _ = raise Format
	in
	    share valueM v
	end

	val valuelist = list valueListM value

	fun con () = let
	    fun c #"1" =
		let
		    val (dc, ts) = dcon ()
		in
		    (F.DATAcon (dc, ts, lvar ()), lexp ())
		end
	      | c #"2" = (F.INTcon (int ()), lexp ())
	      | c #"3" = (F.INT32con (int32 ()), lexp ())
	      | c #"4" = (F.WORDcon (word ()), lexp ())
	      | c #"5" = (F.WORD32con (word32 ()), lexp ())
	      | c #"6" = (F.REALcon (string ()), lexp ())
	      | c #"7" = (F.STRINGcon (string ()), lexp ())
	      | c #"8" = (F.VLENcon (int ()), lexp ())
	      | c _ = raise Format
	in
	    share conM c
	end

	and conlist () = list conListM con ()

	and dcon () = let
	    fun d #"x" = ((symbol (), conrep (), lty ()), tyclist ())
	      | d _ = raise Format
	in
	    share dconM d
	end

	and dict () = let
	    fun d #"y" =
		{ default = lvar (),
		  table = list dictTableM (pair (tyclist, lvar)) () }
	      | d _ = raise Format
	in
	    share dictM d
	end

	and fprim () = let
	    fun f #"z" = (option dictOptionM dict (),
			  primop (), lty (), tyclist ())
	      | f _ = raise Format
	in
	    share fprimM f
	end

	and lexp () = let
	    fun e #"j" = F.RET (valuelist ())
	      | e #"k" = F.LET (lvarlist (), lexp (), lexp ())
	      | e #"l" = F.FIX (fundeclist (), lexp ())
	      | e #"m" = F.APP (value (), valuelist ())
	      | e #"n" = F.TFN (tfundec (), lexp ())
	      | e #"o" = F.TAPP (value (), tyclist ())
	      | e #"p" = F.SWITCH (value (), consig (), conlist (),
				  lexpoption ())
	      | e #"q" = let
		    val (dc, ts) = dcon ()
		in
		    F.CON (dc, ts, value (), lvar (), lexp ())
		end
	      | e #"r" = F.RECORD (rkind (), valuelist (), lvar (), lexp ())
	      | e #"s" = F.SELECT (value (), int (), lvar (), lexp ())
	      | e #"t" = F.RAISE (value (), ltylist ())
	      | e #"u" = F.HANDLE (lexp (), value ())
	      | e #"v" = F.BRANCH (fprim (), valuelist (), lexp (), lexp ())
	      | e #"w" = F.PRIMOP (fprim (), valuelist (), lvar (), lexp ())
	      | e _ = raise Format
	in
	    share lexpM e
	end

	and lexpoption () = option lexpOptionM lexp ()

	and fundec () = let
	    fun f #"a" =
		(fkind (), lvar (), list fdplM (pair (lvar, lty)) (), lexp ())
	      | f _ = raise Format
	in
	    share fundecM f
	end

	and fundeclist () = list fundecListM fundec ()

	and tfundec () = let
	    fun t #"b" = (lvar (), list tfplM (pair (lvar, tkind)) (), lexp ())
	      | t _ = raise Format
	in
	    share tfundecM t
	end

	and fkind () = let
	    fun fk #"2" = F.FK_FCT
	      | fk #"3" = F.FK_FUN { isrec = ltylistoption (),
				     fixed = LT.ffc_var (bool (), bool ()),
				     known = bool (), inline = bool () }
	      | fk #"4" = F.FK_FUN { isrec = ltylistoption (),
				     fixed = LT.ffc_fixed,
				     known = bool (), inline = bool () }
	      | fk _ = raise Format
	in
	    share fkindM fk
	end

	and ltylistoption () = option ltyloM ltylist ()

	and rkind () = let
	    fun rk #"5" = F.RK_VECTOR (tyc ())
	      | rk #"6" = F.RK_STRUCT
	      | rk #"7" = FlintUtil.rk_tuple
	      | rk _ = raise Format
	in
	    share rkindM rk
	end
    in
	fundec
    end

    fun unpickleFLINT pickle = let
	val session =
	    UU.mkSession (UU.stringGetter (Byte.bytesToString pickle))
	val sharedStuff = mkSharedStuff (session, A.LVAR)
	val flint = mkFlintUnpickler (session, sharedStuff)
	val foM = UU.mkMap ()
    in
	UU.r_option session foM flint ()
    end

    fun mkUnpicklers session contexts = let
	val { prim_context, node_context } = contexts
	fun cvtP lk (s, id) =
	    case prim_context s of
		NONE => raise Format
	      | SOME e => (case lk e id of SOME v => v | NONE => raise Format)
	fun cvtN lk (i, s, id) =
	    case node_context (i, s) of
		NONE => raise Format
	      | SOME e => (case lk e id of SOME v => v | NONE => raise Format)
	fun dont i = raise Format
	val c = { lookSTRn = cvtN CMStaticEnv.lookSTR,
		  lookSIGn = cvtN CMStaticEnv.lookSIG,
		  lookFCTn = cvtN CMStaticEnv.lookFCT,
		  lookFSIGn = cvtN CMStaticEnv.lookFSIG,
		  lookTYCn = cvtN CMStaticEnv.lookTYC,
		  lookEENVn = cvtN CMStaticEnv.lookEENV,
		  lookSTRp = cvtP CMStaticEnv.lookSTR,
		  lookSIGp = cvtP CMStaticEnv.lookSIG,
		  lookFCTp = cvtP CMStaticEnv.lookFCT,
		  lookFSIGp = cvtP CMStaticEnv.lookFSIG,
		  lookTYCp = cvtP CMStaticEnv.lookTYC,
		  lookEENVp = cvtP CMStaticEnv.lookEENV,
		  lookSTR = dont,
		  lookSIG = dont,
		  lookFCT = dont,
		  lookFSIG = dont,
		  lookTYC = dont,
		  lookEENV = dont }
	val sharedStuff as { symbol, pid, ... } =
	    mkSharedStuff (session, A.LVAR)
	val symbolListM = UU.mkMap ()
	val symbollist = UU.r_list session symbolListM symbol
	val { envUnpickler', ... } =
	    mkEnvUnpickler (session, symbollist, sharedStuff,
			    c, fn () => raise Format)
	val flint = mkFlintUnpickler (session, sharedStuff)
	val symbind = UU.r_pair (pid, flint)
	val sblM = UU.mkMap ()
	val sbl = UU.r_list session sblM symbind
	fun symenvUnpickler () = SymbolicEnv.fromListi (sbl ())
    in
	{ symenv = symenvUnpickler, env = envUnpickler',
	  symbol = symbol, symbollist = symbollist }
    end

    val unpickleEnv =
	Stats.doPhase (Stats.makePhase "Compiler 087 unpickleEnv") unpickleEnv
end

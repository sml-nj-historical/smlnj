(*
 * The new unpickler (based on the new generic unpickling facility).
 *
 * The unpickler embeds a "modtree" into the unpickled environment.
 * The modtree allows for very rapid construction of modmaps so that
 * modmaps do not have to be stored permanently but can be built on-demand.
 * (Permanently stored modmaps incur space problems: one has to be careful
 * that they don't hang on to bindings that no longer exist, and because
 * of sharing there can be significant overlap--and space overhead--in what
 * each such map points to.  Modtrees do not have these problems.)
 *
 * March 2000, Matthias Blume
 *)
signature UNPICKMOD = sig

    type context = (string list * Symbol.symbol) option -> ModuleId.tmap

    val unpickleEnv : context ->
		      PersStamps.persstamp * Word8Vector.vector ->
		      StaticEnv.staticEnv

    val unpickleFLINT : Word8Vector.vector -> CompBasic.flint option

    (* The env unpickler resulting from "mkUnpicklers" cannot be used for
     * "original" environments that come out of the elaborator.  For those,
     * continue to use "unpickleEnv".  "mkUnpicklers" is intended to be
     * used by CM's stable library mechanism. *)
    val mkUnpicklers :
	{ session: UnpickleUtil.session,
	  stringlist: string list UnpickleUtil.reader } ->
	context ->
	{ symenv: SymbolicEnv.symenv UnpickleUtil.reader,
	  statenv: StaticEnv.staticEnv UnpickleUtil.reader,
	  symbol: Symbol.symbol UnpickleUtil.reader,
	  symbollist: Symbol.symbol list UnpickleUtil.reader }
end

structure UnpickMod : UNPICKMOD = struct

    type context = (string list * Symbol.symbol) option -> ModuleId.tmap

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

    fun & c (x, t) = (c x, t)

    fun branch l = let
	fun loop ([], [x]) = x
	  | loop ([], l) = M.BRANCH l
	  | loop (M.BRANCH [] :: t, l) = loop (t, l)
	  | loop (M.BRANCH [x] :: t, l) = loop (t, x :: l) (* never occurs! *)
	  | loop (x :: t, l) = loop (t, x :: l)
    in
	loop (l, [])
    end

    val notree = M.BRANCH []

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
	val nkM = UU.mkMap ()
	val poM = UU.mkMap ()
	val boolListM = UU.mkMap ()
	val tkindM = UU.mkMap ()
	val tkindListM = UU.mkMap ()

	val boollist = list boolListM bool

	val pid = UnpickleSymPid.r_pid (session, string)
	    
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

	fun tkind () = let
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
	  primop = primop, boollist = boollist,
	  tkind = tkind, tkindlist = tkindlist }
    end

    fun mkEnvUnpickler extraInfo sessionInfo context = let
	val { globalPid, symbollist, sharedStuff, lib } = extraInfo
	val { session, stringlist } = sessionInfo

	local
	    fun look lk (m, i) =
		case lk (context m, i) of
		    SOME x => x
		  | NONE =>
		    (ErrorMsg.impossible "UnpickMod: stub lookup failed";
		     raise Format)
	in
	    val lookTyc = look MI.lookTyc
	    val lookSig = look MI.lookSig
	    val lookStr = look MI.lookStr
	    val lookFct = look MI.lookFct
	    val lookEnv = look MI.lookEnv
	end

	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r
	val bool = UU.r_bool session
	fun pair m fp p = UU.r_pair session m fp p
	val int = UU.r_int session

	fun share m f = UU.share session m f
	fun nonshare f = UU.nonshare session f

	(* The following maps all acquire different types by being used
	 * in different contexts: *)
	val stampM = UU.mkMap ()
	val strIdM = UU.mkMap ()
	val fctIdM = UU.mkMap ()
	val stampOptionM = UU.mkMap ()
	val stampListM = UU.mkMap ()
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
	val spathM = UU.mkMap ()
	val ipathM = UU.mkMap ()
	val symSpecPM = UU.mkMap ()
	val epTkPM = UU.mkMap ()
	val sdIntPM = UU.mkMap ()
	val evEntPM = UU.mkMap ()
	val symBindPM = UU.mkMap ()
	val pidOptionM = UU.mkMap ()
	val lmsOptM = UU.mkMap ()
	val lmsPairM = UU.mkMap ()

	val { pid, string, symbol, access, conrep, consig,
	      primop, boollist, tkind, tkindlist } = sharedStuff

	fun libModSpec () =
	    option lmsOptM (pair lmsPairM (stringlist, symbol)) ()

	fun stamp () = let
	    fun st #"A" = Stamps.global { pid = globalPid (),
					  cnt = int () }
	      | st #"B" = Stamps.global { pid = pid (),
					  cnt = int () }
	      | st #"C" = Stamps.special (string ())
	      | st _ = raise Format
	in
	    share stampM st
	end    

	val tycId = stamp
	val sigId = stamp
	fun strId () = let
	    fun si #"D" = { sign = stamp (), rlzn = stamp () }
	      | si _ = raise Format
	in
	    share strIdM si
	end
	fun fctId () = let
	    fun fi #"E" = { paramsig = stamp (), bodysig = stamp (),
			    rlzn = stamp () }
	      | fi _ = raise Format
	in
	    share fctIdM fi
	end
	val envId = stamp

	val stamplist = list stampListM stamp
	val stampoption = option stampOptionM stamp
	val pidoption = option pidOptionM pid

	val entVar = stamp
	val entVarOption = stampoption
	val entPath = stamplist

	val symbollist = list symbolListM symbol
	val symboloption = option symbolOptionM symbol

	fun spath () = let
	    fun sp #"s" = SP.SPATH (symbollist ())
	      | sp _ = raise Format
	in
	    share spathM sp
	end

	fun ipath () = let
	    fun ip #"i" = IP.IPATH (symbollist ())
	      | ip _ = raise Format
	in
	    share ipathM ip
	end

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

	fun datacon' () = let
	    fun d #"c" =
		let val n = symbol ()
		    val c = bool ()
		    val (t, ttr) = ty' ()
		    val r = conrep ()
		    val s = consig ()
		    val l = bool ()
		in
		    (T.DATACON { name = n, const = c, typ = t,
				 rep = r, sign = s, lazyp = l },
		     ttr)
		end
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
	    fun tyc #"A" = T.GENtyc (lookTyc (libModSpec (), tycId ()))
	      | tyc #"B" = T.GENtyc { stamp = stamp (),
				      arity = int (),
				      eq = ref (eqprop ()),
				      kind = tyckind (),
				      path = ipath (),
				      stub = SOME { owner = if lib then pid ()
							    else globalPid (),
						    lib = lib } }
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
	      | tyc _ = raise Format
	in
	    share tyconM tyc
	end

	and tycon' () = let
	    val tyc = tycon ()
	    val tree =
		case tyc of
		    T.GENtyc r => M.TYCNODE r
		  | _ => notree
	in
	    (tyc, tree)
	end

	and tyconlist () = list tyconListM tycon ()

	and ty' () = let
	    fun t #"a" =
		let val (tyc, tyctr) = tycon' ()
		    val (tyl, tyltr) = tylist' ()
		in (T.CONty (tyc, tyl), branch [tyctr, tyltr])
		end
	      | t #"b" = (T.IBOUND (int ()), notree)
	      | t #"c" = (T.WILDCARDty, notree)
	      | t #"d" =
		let val s = boollist ()
		    val ar = int ()
		    val (b, btr) = ty' ()
		in
		    (T.POLYty { sign = s, tyfun = T.TYFUN { arity = ar,
							    body = b } },
		     btr)
		end
	      | t #"e" = (T.UNDEFty, notree)
	      | t _ = raise Format
	in
	    share tyM t
	end

	and ty () = #1 (ty' ())

	and tyoption () = option tyOptionM ty ()

	and tylist' () = let
	    val (l, trl) = ListPair.unzip (list tyListM ty' ())
	in
	    (l, branch trl)
	end

	and inl_info () = let
	    fun ii #"A" = II.INL_PRIM (primop (), tyoption ())
	      | ii #"B" = II.INL_STR (iilist ())
	      | ii #"C" = II.INL_NO
	      | ii _ = raise Format
	in
	    share iiM ii
	end

	and iilist () = list iiListM inl_info ()

	and var' () = let
	    fun v #"1" =
		let val a = access ()
		    val i = inl_info ()
		    val p = spath ()
		    val (t, tr) = ty' ()
		in
		    (V.VALvar { access = a, info = i, path = p, typ = ref t },
		     tr)
		end
	      | v #"2" =
		let val n = symbol ()
		    val (ol, oltr) = overldlist' ()
		    val ar = int ()
		    val (b, btr) = ty' ()
		in
		    (V.OVLDvar { name = n,
				 options = ref ol,
				 scheme = T.TYFUN { arity = ar, body = b } },
		     branch [oltr, btr])
		end
	      | v #"3" = (V.ERRORvar, notree)
	      | v _ = raise Format
	in
	    share vM v
	end

	and overld' () = let
	    fun ov #"o" =
		let val (t, ttr) = ty' ()
		    val (v, vtr) = var' ()
		in
		    ({ indicator = t, variant = v },
		     branch [ttr, vtr])
		end
	      | ov _ = raise Format
	in
	    share overldM ov
	end

	and overldlist' () = let
	    val (l, trl) = ListPair.unzip (list olListM overld' ())
	in
	    (l, branch trl)
	end

	fun strDef () = let
	    fun sd #"C" = M.CONSTstrDef (Structure ())
	      | sd #"V" = M.VARstrDef (Signature (), entPath ())
	      | sd _ = raise Format
	in
	    share sdM sd
	end

	and Signature' () = let
	    fun sg #"A" = (M.ERRORsig, notree)
	      | sg #"B" =
		let val sr = lookSig (libModSpec (), sigId ())
		in
		    (M.SIG sr, M.SIGNODE sr)
		end
	      | sg #"C" =
		let val s = stamp ()
		    val n = symboloption ()
		    val c = bool ()
		    val ff = bool ()
		    val sl = symbollist ()
		    val (el, eltrl) =
			ListPair.unzip
			    (map (fn (sy, (sp, tr)) => ((sy, sp), tr))
			         (list elementsM
				  (pair symSpecPM (symbol, spec')) ()))
		    val beps = option bepsOM
				      (list bepsLM
					    (pair epTkPM (entPath, tkind))) ()
		    val ts = spathlistlist ()
		    val ss = spathlistlist ()
		    val r = { stamp = s,
			      name = n,
			      closed = c,
			      fctflag = ff,
			      symbols = sl,
			      elements = el,
			      boundeps = ref beps,
			      lambdaty = ref NONE,
			      typsharing = ts,
			      strsharing = ss,
			      stub = SOME { owner = if lib then pid ()
						    else globalPid (),
					    tree = branch eltrl,
					    lib = lib } }
		in
		    (M.SIG r, M.SIGNODE r)
		end
	      | sg _ = raise Format
	in
	    share sigM sg
	end

	and Signature () = #1 (Signature' ())

	and fctSig' () = let
	    fun fsg #"a" = (M.ERRORfsig, notree)
	      | fsg #"c" =
		let val k = symboloption ()
		    val (ps, pstr) = Signature' ()
		    val pv = entVar ()
		    val psy = symboloption ()
		    val (bs, bstr) = Signature' ()
		in
		    (M.FSIG { kind = k, paramsig = ps,
			      paramvar = pv, paramsym = psy,
			      bodysig = bs },
		     branch [pstr, bstr])
		end
	      | fsg _ = raise Format
	in
	    share fsigM fsg
	end

	and spec' () = let
	    val intoption = option ioM int
	    fun sp #"1" =
		let val (t, ttr) = tycon' ()
		in
		    (M.TYCspec { spec = t, entVar = entVar (),
				 repl = bool (), scope = int () },
		     ttr)
		end
	      | sp #"2" =
		let val (s, str) = Signature' ()
		in
		    (M.STRspec { sign = s, slot = int (),
				 def = option spDefM
				              (pair sdIntPM (strDef, int)) (),
				 entVar = entVar () },
		     str)
		end
	      | sp #"3" =
		let val (f, ftr) = fctSig' ()
		in
		    (M.FCTspec { sign = f, slot = int (), entVar = entVar () },
		     ftr)
		end
	      | sp #"4" =
		let val (t, ttr) = ty' ()
		in
		    (M.VALspec { spec = t, slot = int () }, ttr)
		end
	      | sp #"5" =
		let val (d, dtr) = datacon' ()
		in
		    (M.CONspec { spec = d, slot = intoption () }, dtr)
		end
	      | sp _ = raise Format
	in
	    share spM sp
	end

	and entity' () = let
	    fun en #"A" = & M.TYCent (tycEntity' ())
	      | en #"B" = & M.STRent (strEntity' ())
	      | en #"C" = & M.FCTent (fctEntity' ())
	      | en #"D" = (M.ERRORent, notree)
	      | en _ = raise Format
	in
	    share enM en
	end

	and fctClosure' () = let
	    fun f #"f" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		    val (e, etr) = entityEnv' ()
		in
		    (M.CLOSURE { param = p, body = b, env = e },
		     branch [btr, etr])
		end
	      | f _ = raise Format
	in
	    share fctcM f
	end

	(* The construction of the STRNODE in the modtree deserves some
	 * comment:  Even though it contains the whole strrec, it does
	 * _not_ take care of the Signature contained therein.  The reason
	 * why STRNODE has the whole strrec and not just the strEntity that
	 * it really guards is that the identity of the strEntity is not
	 * fully recoverable without also having access to the Signature.
	 * The same situation occurs in the case of FCTNODE. *)
	and Structure' () = let
	    fun str #"A" =
		let val (s, str) = Signature' ()
		in
		    (M.STRSIG { sign = s, entPath = entPath () }, str)
		end
	      | str #"B" = (M.ERRORstr, notree)
	      | str #"C" =
		let val (s, str) = Signature' ()
		    val r = { sign = s,
			      rlzn = lookStr (libModSpec (), strId ()),
			      access = access (),
			      info = inl_info () }
		in
		    (M.STR r, branch [str, M.STRNODE r])
		end
	      | str #"D" =
		let val (s, str) = Signature' ()
		    val r = { sign = s,
			      rlzn = strEntity (),
			      access = access (),
			      info = inl_info () }
		in
		    (M.STR r, branch [str, M.STRNODE r])
		end
	      | str _ = raise Format
	in
	    share strM str
	end

	and Structure () = #1 (Structure' ())

	(* See the comment about STRNODE, strrec, Signature, and strEntity
	 * in front of Structure'.  The situation for FCTNODE, fctrec,
	 * fctSig, and fctEntity is analogous. *)
	and Functor' () = let
	    fun fct #"E" = (M.ERRORfct, notree)
	      | fct #"F" =
		let val (s, str) = fctSig' ()
		    val r = { sign = s,
			      rlzn = lookFct (libModSpec (), fctId ()),
			      access = access (),
			      info = inl_info () }
		in
		    (M.FCT r, branch [str, M.FCTNODE r])
		end
	      | fct #"G" =
		let val (s, str) = fctSig' ()
		    val r = { sign = s,
			      rlzn = fctEntity (),
			      access = access (),
			      info = inl_info () }
		in
		    (M.FCT r, branch [str, M.FCTNODE r])
		end
	      | fct _ = raise Format
	in
	    share fctM fct
	end

	and stampExp () = let
	    fun ste #"b" = M.GETSTAMP (strExp ())
	      | ste #"c" = M.NEW
	      | ste _ = raise Format
	in
	    share steM ste
	end

	and tycExp' () = let
	    fun tce #"d" = & M.CONSTtyc (tycon' ())
	      | tce #"e" = (M.FORMtyc (tycon ()), notree) (* ? *)
	      | tce #"f" = (M.VARtyc (entPath ()), notree)
	      | tce _ = raise Format
	in
	    share tceM tce
	end

	and tycExp () = #1 (tycExp' ())

	and strExp' () = let
	    fun stre #"g" = (M.VARstr (entPath ()), notree)
	      | stre #"h" = & M.CONSTstr (strEntity' ())
	      | stre #"i" =
		let val s = stampExp ()
		    val (d, dtr) = entityDec' ()
		in
		    (M.STRUCTURE { stamp = s, entDec = d }, dtr)
		end
	      | stre #"j" =
		let val (f, ftr) = fctExp' ()
		    val (s, str) = strExp' ()
		in
		    (M.APPLY (f, s), branch [ftr, str])
		end
	      | stre #"k" =
		let val (d, dtr) = entityDec' ()
		    val (s, str) = strExp' ()
		in
		    (M.LETstr (d, s), branch [dtr, str])
		end
	      | stre #"l" =
		let val (s, str) = Signature' ()
		    val (e, etr) = strExp' ()
		in
		    (M.ABSstr (s, e), branch [str, etr])
		end
	      | stre #"m" =
		let val bv = entVar ()
		    val (r, rtr) = strExp' ()
		    val (c, ctr) = strExp' ()
		in
		    (M.CONSTRAINstr { boundvar = bv, raw = r, coercion = c },
		     branch [rtr, ctr])
		end
	      | stre #"n" = & M.FORMstr (fctSig' ())
	      | stre _ = raise Format
	in
	    share streM stre
	end

	and strExp () = #1 (strExp' ())

	and fctExp' () = let
	    fun fe #"o" = (M.VARfct (entPath ()), notree)
	      | fe #"p" = & M.CONSTfct (fctEntity' ())
	      | fe #"q" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		in
		    (M.LAMBDA { param = p, body = b }, btr)
		end
	      | fe #"r" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		    val (s, str) = fctSig' ()
		in
		    (M.LAMBDA_TP { param = p, body = b, sign = s },
		     branch [btr, str])
		end
	      | fe #"s" =
		let val (d, dtr) = entityDec' ()
		    val (f, ftr) = fctExp' ()
		in
		    (M.LETfct (d, f), branch [dtr, ftr])
		end
	      | fe _ = raise Format
	in
	    share feM fe
	end

	and fctExp () = #1 (fctExp' ())

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

	and entityDec' () = let
	    fun ed #"A" =
		let val v = entVar ()
		    val (e, etr) = tycExp' ()
		in
		    (M.TYCdec (v, e), etr)
		end
	      | ed #"B" =
		let val v = entVar ()
		    val (e, etr) = strExp' ()
		    val s = symbol ()
		in
		    (M.STRdec (v, e, s), etr)
		end
	      | ed #"C" =
		let val v = entVar ()
		    val (e, etr) = fctExp' ()
		in
		    (M.FCTdec (v, e), etr)
		end
	      | ed #"D" = & M.SEQdec (entityDecList' ())
	      | ed #"E" =
		let val (d1, d1tr) = entityDec' ()
		    val (d2, d2tr) = entityDec' ()
		in
		    (M.LOCALdec (d1, d2), branch [d1tr, d2tr])
		end
	      | ed #"F" = (M.ERRORdec, notree)
	      | ed #"G" = (M.EMPTYdec, notree)
	      | ed _ = raise Format
	in
	    share edM ed
	end

	and entityDecList' () = let
	    val (l, trl) = ListPair.unzip (list edListM entityDec' ())
	in
	    (l, branch trl)
	end

	and entityEnv' () = let
	    fun eenv #"A" =
		let val l = list eenvBindM (pair evEntPM (entVar, entity')) ()
		    val l' = map (fn (v, (e, tr)) => ((v, e), tr)) l
		    val (l'', trl) = ListPair.unzip l'
		    fun add ((v, e), z) = ED.insert (z, v, e)
		    val ed = foldr add ED.empty l''
		    val (e, etr) = entityEnv' ()
		in
		    (M.BINDeenv (ed, e), branch (etr :: trl))
		end
	      | eenv #"B" = (M.NILeenv, notree)
	      | eenv #"C" = (M.ERReenv, notree)
	      | eenv #"D" =
		let val r = lookEnv (libModSpec (), envId ())
		in
		    (M.MARKeenv r, M.ENVNODE r)
		end
	      | eenv #"E" =
		let val s = stamp ()
		    val (e, etr) = entityEnv' ()
		    val r = { stamp = s,
			      env = e,
			      stub = SOME { owner = if lib then pid ()
						    else globalPid (),
					    tree = etr,
					    lib = lib } }
		in
		    (M.MARKeenv r, M.ENVNODE r)
		end
	      | eenv _ = raise Format
	in
	    share eenvM eenv
	end

	and strEntity' () = let
	    fun s #"s" =
		let val s = stamp ()
		    val (e, etr) = entityEnv' ()
		in
		    ({ stamp = s,
		       entities = e,
		       rpath = ipath (),
		       lambdaty = ref NONE,
		       stub = SOME { owner = if lib then pid ()
					     else globalPid (),
				     tree = etr,
				     lib = lib } },
		     etr)
		end
	      | s _ = raise Format
	in
	    share senM s
	end

	and strEntity () = #1 (strEntity' ())

	and fctEntity' () = let
	    fun f #"f" =
		let val s = stamp ()
		    val (c, ctr) = fctClosure' ()
		in
		    ({ stamp = s,
		       closure = c,
		       rpath = ipath (),
		       lambdaty = ref NONE,
		       tycpath = NONE,
		       stub = SOME { owner = if lib then pid ()
					     else globalPid (),
				     tree = ctr,
				     lib = lib } },
		     ctr)
		end
	      | f _ = raise Format
	in
	    share fenM f
	end

	and fctEntity () = #1 (fctEntity' ())

	and tycEntity' () = tycon' ()

	fun fixity () = let
	    fun fx #"N" = Fixity.NONfix
	      | fx #"I" = Fixity.INfix (int (), int ())
	      | fx _ = raise Format
	in
	    share fxM fx
	end

	fun binding' () = let
	    fun b #"1" = & B.VALbind (var' ())
	      | b #"2" = & B.CONbind (datacon' ())
	      | b #"3" = & B.TYCbind (tycon' ())
	      | b #"4" = & B.SIGbind (Signature' ())
	      | b #"5" = & B.STRbind (Structure' ())
	      | b #"6" = & B.FSGbind (fctSig' ())
	      | b #"7" = & B.FCTbind (Functor' ())
	      | b #"8" = (B.FIXbind (fixity ()), notree)
	      | b _ = raise Format
	in
	    share bM b
	end

	fun env () = let
	    val bindlist = list envM (pair symBindPM (symbol, binding')) ()
	    fun bind ((s, (b, t)), e) = StaticEnv.bind0 (s, (b, SOME t), e)
	in
	    Env.consolidate (foldl bind StaticEnv.empty bindlist)
	end
    in
	env
    end

    fun unpickleEnv context (hash, pickle) = let
	val session =
	    UU.mkSession (UU.stringGetter (Byte.bytesToString pickle))
	fun import i = A.PATH (A.EXTERN hash, i)
	val slM = UU.mkMap ()
	val sloM = UU.mkMap ()
	val sylM = UU.mkMap ()
	val sharedStuff = mkSharedStuff (session, import)
	val stringlist = UU.r_list session slM (#string sharedStuff)
	val symbollist = UU.r_list session sylM (#symbol sharedStuff)
	val extraInfo = { globalPid = fn () => hash,
			  symbollist = symbollist,
			  sharedStuff = sharedStuff,
			  lib = false }
	val sessionInfo = { session = session, stringlist = stringlist }
	val unpickle = mkEnvUnpickler extraInfo sessionInfo context
    in
	unpickle ()
    end

    fun mkFlintUnpickler (session, sharedStuff) = let

	fun share m r = UU.share session m r

	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r

	fun pair m fp p = UU.r_pair session m fp p
	val int = UU.r_int session
	val int32 = UU.r_int32 session
	val word = UU.r_word session
	val word32 = UU.r_word32 session
	val bool = UU.r_bool session

	val { pid, string, symbol, access, conrep, consig,
	      primop, boollist, tkind, tkindlist } = sharedStuff

	val ltyM = UU.mkMap ()
	val ltyListM = UU.mkMap ()
	val tycM = UU.mkMap ()
	val tycListM = UU.mkMap ()
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
	val lvLtPM = UU.mkMap ()
	val lvLtPLM = UU.mkMap ()
	val lvTkPM = UU.mkMap ()
	val lvTkPLM = UU.mkMap ()
	val tycLvPM = UU.mkMap ()

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
	      | tc #"B" = LT.tcc_nvar (int ())
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
		  table = list dictTableM (pair tycLvPM (tyclist, lvar)) () }
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
		(fkind (), lvar (),
		 list lvLtPLM (pair lvLtPM (lvar, lty)) (),
		 lexp ())
	      | f _ = raise Format
	in
	    share fundecM f
	end

	and fundeclist () = list fundecListM fundec ()

	and tfundec () = let
	    fun t #"b" = ({ inline = F.IH_SAFE }, lvar (),
			  list lvTkPLM (pair lvTkPM (lvar, tkind)) (),
			  lexp ())
	      | t _ = raise Format
	in
	    share tfundecM t
	end

	and fkind () = let
	    fun aug_unknown x = (x, F.LK_UNKNOWN)
	    fun inlflag true = F.IH_ALWAYS
	      | inlflag false = F.IH_SAFE
	    fun fk #"2" = { isrec = NONE, cconv = F.CC_FCT,
			    known = false, inline = F.IH_SAFE }
	      | fk #"3" = { isrec = Option.map aug_unknown (ltylistoption ()),
			    cconv = F.CC_FUN (LT.ffc_var (bool (), bool ())),
			    known = bool (),
			    inline = inlflag (bool ()) }
	      | fk #"4" = { isrec = Option.map aug_unknown (ltylistoption ()),
			    cconv = F.CC_FUN LT.ffc_fixed,
			    known = bool (),
			    inline = inlflag (bool ()) }
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

    fun mkUnpicklers sessionInfo context = let
	val { session, stringlist } = sessionInfo
	val sharedStuff = mkSharedStuff (session, A.LVAR)
	val { symbol, pid, ... } = sharedStuff
	val sylM = UU.mkMap ()
	val symbollist = UU.r_list session sylM symbol
	val extraInfo = { globalPid = fn () => raise Format,
			  symbollist = symbollist,
			  sharedStuff = sharedStuff,
			  lib = true }
	val statenv = mkEnvUnpickler extraInfo sessionInfo context
	val flint = mkFlintUnpickler (session, sharedStuff)
	val pidFlintPM = UU.mkMap ()
	val symbind = UU.r_pair session pidFlintPM (pid, flint)
	val sblM = UU.mkMap ()
	val sbl = UU.r_list session sblM symbind
	fun symenv () = SymbolicEnv.fromListi (sbl ())
    in
	{ symenv = symenv, statenv = statenv,
	  symbol = symbol, symbollist = symbollist }
    end

    val unpickleEnv =
	fn c => Stats.doPhase (Stats.makePhase "Compiler 087 unpickleEnv")
			      (unpickleEnv c)
end

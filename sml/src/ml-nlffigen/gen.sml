(*
 * gen.sml - Generating and pretty-printing ML code implementing a
 *           typed interface to a C program.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
local
    val program = "ml-ffigen"
    val version = "0.1"
    val author = "Matthias Blume"
    val email = "blume@research.bell-labs.com"
    structure S = Spec
in

structure Gen :> sig
    val gen : { idlfile: string,
		idlsource: string,
		sigfile: string,
		strfile: string,
		cmfile:  string,
		signame: string,
		strname: string,
		allSU: bool,
		lambdasplit: string option,
		wid: int } -> unit
end = struct

    structure P = PrettyPrint
    structure PP = P.PP
    val Tuple = P.TUPLE
    val Con = P.CON
    val Arrow = P.ARROW
    val Type = P.Type
    val St = P.St
    val Un = P.Un
    val Unit = P.Unit
    val ETuple = P.ETUPLE
    val EVar = P.EVAR
    val EApp = P.EAPP
    val EConstr = P.ECONSTR
    val ESeq = P.ESEQ
    fun EWord w = EVar ("0wx" ^ Word.toString w)
    fun EInt i = EVar (Int.toString i)
    fun EString s = EVar (concat ["\"", String.toString s, "\""])

    val dontedit = "(* This file has been generated automatically. \
		   \DO NOT EDIT! *)"
    fun mkCredits src = concat ["(* [from ", src, " by ", author, "'s ",
				program, " (version ", version, ")] *)"]
    val commentsto = concat ["(* Send comments and suggestions to ",
			     email, ". Thanks! *)"]

    fun gen args = let

	val { idlfile, idlsource,
	      sigfile, strfile, cmfile,
	      signame, strname,
	      allSU, lambdasplit,
	      wid } = args

	val credits = mkCredits idlfile

	val astbundle = ParseToAst.fileToAst'
			    TextIO.stdErr
			    (GenSizes.sizes, State.INITIAL)
			    idlsource

	val spec = AstToSpec.build (astbundle, GenSizes.sizes, idlfile, allSU)

	val { structs, unions, gvars, gfuns, gtys } = spec

	fun openPP f =
	    PP.openStream (SimpleTextIODev.openDev { dst = TextIO.openOut f,
						     wid = wid })

	exception Incomplete

	fun get_struct t =
	    case List.find (fn s => #tag s = t) structs of
		SOME x => x
	      | NONE => raise Incomplete
	fun get_union t =
	    case List.find (fn u => #tag u = t) unions of
		SOME x => x
	      | NONE => raise Incomplete

	fun stem S.SCHAR = "schar"
	  | stem S.UCHAR = "uchar"
	  | stem S.SINT = "sint"
	  | stem S.UINT = "uint"
	  | stem S.SSHORT = "sshort"
	  | stem S.USHORT = "ushort"
	  | stem S.SLONG = "slong"
	  | stem S.ULONG = "ulong"
	  | stem S.FLOAT = "float"
	  | stem S.DOUBLE = "double"
	  | stem S.VOIDPTR = "voidptr"
	  | stem _ = raise Fail "bad stem"

	fun sinsert (s: string, l) =
	    case List.find (fn s' => s = s') l of
		SOME _ => l
	      | NONE => s :: l

	(* We don't expect many different function pointer types or
	 * incomplete types in any given C interface, so using linear
	 * lists here is probably ok. *)
	val (fptr_types, incomplete_structs, incomplete_unions) = let
	    fun ty ((S.SCHAR | S.UCHAR | S.SINT | S.UINT |
		     S.SSHORT | S.USHORT |
		     S.SLONG | S.ULONG | S.FLOAT | S.DOUBLE |
		     S.VOIDPTR), a) = a
	      | ty (S.STRUCT t, a as (f, s, u)) =
		((ignore (get_struct t); a)
		 handle Incomplete => (f, sinsert (t, s), u))
	      | ty (S.UNION t, a as (f, s, u)) =
		((ignore (get_union t); a)
		 handle Incomplete => (f, s, sinsert (t, u)))
	      | ty ((S.PTR (_, t) | S.ARR { t, ... }), a) = ty (t, a)
	      | ty (S.FPTR cft, a as (f, s, u)) =
		if List.exists (fn (cft', _) => cft = cft') f then a
		else ((cft, length f) :: f, s, u)
	    fun fs (S.OFIELD { spec = (_, t), ... }, a) = ty (t, a)
	      | fs (_, a) = a
	    fun f ({ name, spec }, a) = fs (spec, a)
	    fun s ({ tag, size, anon, fields }, a) = foldl f a fields
	    fun u ({ tag, size, anon, largest, all }, a) =
		foldl f a (largest :: all)
	    fun gty ({ name, spec }, a) = ty (spec, a)
	    fun gvar ({ name, spec = (_, t) }, a) = ty (t, a)
	    fun gfun ({ name, spec }, a) = ty (S.FPTR spec, a)
	in
	    foldl gfun (foldl gvar
		         (foldl gty (foldl u (foldl s ([], [], []) structs)
					   unions)
				gtys)
			 gvars)
		  gfuns
	end

	fun incomplete t = let
	    fun decide (K, tag: Spec.tag, l) =
		if List.exists (fn tag' => tag = tag') l then
		    SOME (K, tag)
		else NONE
	in
	    case t of
		S.STRUCT tag => decide ("S", tag, incomplete_structs)
	      | S.UNION tag => decide ("U", tag, incomplete_unions)
	      | _ => NONE
	end

	fun istruct (K, tag) = concat ["I_", K, "_", tag]

	fun rwro S.RW = Type "rw"
	  | rwro S.RO = Type "ro"

	fun dim_ty 0 = Type "dec"
	  | dim_ty n = Con ("dg" ^ Int.toString (n mod 10),
			    [dim_ty (n div 10)])

	fun Suobj'rw sut = Con ("su_obj'", [sut, Type "rw"])
	fun Suobj'ro sut = Con ("su_obj'", [sut, Type "ro"])
	fun Suobj''c sut = Con ("su_obj'", [sut, Type "'c"])

	fun wtn_f_fptr_p p { args, res } = let
	    fun topty (S.STRUCT t) = Suobj'ro (St t)
	      | topty (S.UNION t) = Suobj'ro (Un t)
	      | topty t = wtn_ty' t
	    val (res_t, extra_arg_t) =
		case res of
		    NONE => (Unit, [])
		  | SOME (S.STRUCT t) => let
			val ot = Suobj'rw (St t)
		    in
			(ot, [ot])
		    end
		  | SOME (S.UNION t) => let
			val ot = Suobj'rw (Un t)
		    in
			(ot, [ot])
		    end
		  | SOME t => (topty t, [])
	    val arg_tl = extra_arg_t @ map topty args
	    val dom_t = Tuple arg_tl
	    val fct_t = Arrow (dom_t, res_t)
	in
	    (Con ("fptr" ^ p, [fct_t]), fct_t)
	end

	and wtn_f_ty_p p (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				S.FLOAT | S.DOUBLE | S.VOIDPTR)) =
	    (Type (stem t), Unit)
	  | wtn_f_ty_p p (S.STRUCT t) = (Con ("su", [St t]), Unit)
	  | wtn_f_ty_p p (S.UNION t) = (Con ("su", [Un t]), Unit)
	  | wtn_f_ty_p p (S.PTR (c, t)) =
	    (case incomplete t of
		 SOME (K, tag) =>
		 (Con (concat [istruct (K, tag), ".iptr", p], [rwro c]), Unit)
	       | NONE => let
		     val (w, f) = wtn_f_ty t
		 in
		     (Con ("ptr" ^ p, [w, f, rwro c]), f)
		 end)
	  | wtn_f_ty_p p (S.ARR { t, d, ... }) = let
		val (w, f) = wtn_f_ty t
	    in
		(Con ("arr", [w, f, dim_ty d]), f)
	    end
	  | wtn_f_ty_p p (S.FPTR spec) = wtn_f_fptr_p p spec

	and wtn_f_ty t = wtn_f_ty_p "" t

	and wtn_ty t = #1 (wtn_f_ty t)

	and wtn_ty' t = #1 (wtn_f_ty_p "'" t)

	fun topfunc_ty p { args, res } = let
	    fun topty S.SCHAR = Type "MLRep.SChar.int"
	      | topty S.UCHAR = Type "MLRep.UChar.word"
	      | topty S.SINT = Type "MLRep.SInt.int"
	      | topty S.UINT = Type "MLRep.UInt.word"
	      | topty S.SSHORT = Type "MLRep.SShort.int"
	      | topty S.USHORT = Type "MLRep.UShort.word"
	      | topty S.SLONG = Type "MLRep.SLong.int"
	      | topty S.ULONG = Type "MLRep.ULong.word"
	      | topty S.FLOAT = Type "MLRep.Float.real"
	      | topty S.DOUBLE = Type "MLRep.Double.real"
	      | topty (S.STRUCT t) = Con ("su_obj" ^ p, [St t, Type "'c"])
	      | topty (S.UNION t) = Con ("su_obj" ^ p, [Un t, Type "'c"])
	      | topty t = #1 (wtn_f_ty_p p t)
	    val (res_t, extra_arg_t) =
		case res of
		    NONE => (Unit, [])
		  | SOME (S.STRUCT t) => let
			val ot = Suobj'rw (St t)
		    in
			(ot, [ot])
		    end
		  | SOME (S.UNION t) => let
			val ot = Suobj'rw (Un t)
		    in
			(ot, [ot])
		    end
		  | SOME t => (topty t, [])
	in
	    Arrow (Tuple (extra_arg_t @ map topty args), res_t)
	end

	fun  rti_ty t = let
	    val (w, f) = wtn_f_ty t
	in
	    Con ("T.typ", [w, f])
	end

	fun  obj_ty p (t, c) = let
	    val (w, f) = wtn_f_ty t
	in
	    Con ("obj" ^ p, [w, f, c])
	end

	fun cro S.RW = Type "'c"
	  | cro S.RO = Type "ro"

	fun dim_val n = let
	    fun build 0 = EVar "dec"
	      | build n = EApp (build (n div 10),
				EVar ("dg" ^ Int.toString (n mod 10)))
	in
	    EApp (build n, EVar "dim")
	end

	local
	    fun simple v = EVar ("T." ^ v)
	in
	    fun rti_val (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			       S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			       S.FLOAT | S.DOUBLE | S.VOIDPTR)) =
		simple (stem t)
	      | rti_val (S.STRUCT t) = EVar (concat ["S_", t, ".typ"])
	      | rti_val (S.UNION t) = EVar (concat ["U_", t, ".typ"])
	      | rti_val (S.FPTR cft) =
		(case List.find (fn x => #1 x = cft) fptr_types of
		     SOME (_, i) => EVar ("fptr_rti_" ^ Int.toString i)
		   | NONE => raise Fail "fptr type missing")
	      | rti_val (S.PTR (S.RW, t)) =
		(case incomplete t of
		     SOME (K, tag) =>
		     EVar (istruct (K, tag) ^ ".typ'rw")
		   | NONE => EApp (EVar "T.pointer", rti_val t))
	      | rti_val (S.PTR (S.RO, t)) =
		(case incomplete t of
		     SOME (K, tag) =>
		     EVar (istruct (K, tag) ^ ".typ'ro")
		   | NONE => EApp (EVar "T.ro",
				   EApp (EVar "T.pointer", rti_val t)))
	      | rti_val (S.ARR { t, d, ... }) =
		EApp (EVar "T.arr", ETuple [rti_val t, dim_val d])
	end

	fun do_sig_file () = let

	    val sigpp = openPP sigfile

	    fun nl () = PP.newline sigpp
	    fun str s = PP.string sigpp s
	    fun sp () = PP.space sigpp 1
	    fun nsp () = PP.nbSpace sigpp 1
	    fun Box a = PP.openBox sigpp (PP.Abs a)
	    fun HBox () = PP.openHBox sigpp
	    fun HOVBox a = PP.openHOVBox sigpp (PP.Abs a)
	    fun VBox a = PP.openVBox sigpp (PP.Abs a)
	    fun endBox () = PP.closeBox sigpp
	    fun ppty t = P.ppType sigpp t
			  
	    fun pr_su_tag t =
		(nl (); HBox (); str "type"; sp (); ppty t; endBox ())

	    fun pr_struct_tag { tag, size, anon, fields } =
		pr_su_tag (St tag)

	    fun pr_union_tag { tag, size, anon, largest, all } =
		pr_su_tag (Un tag)

	    fun pr_decl (keyword, connector) (v, t) =
		(nl (); HOVBox 4; str keyword; nsp (); str v; nsp ();
		 str connector; sp (); ppty t; endBox ())

	    val pr_tdef = pr_decl ("type", "=")
	    val pr_vdecl = pr_decl ("val", ":")

	    fun pr_su_structure (StUn, K, su, tag, fields) = let

		fun pr_field_typ { name, spec = S.OFIELD { spec = (c, t),
							   synthetic = false,
							   offset } } =
		    pr_tdef ("t_f_" ^ name, wtn_ty t)
		  | pr_field_typ _ = ()

		fun pr_field_rti { name, spec = S.OFIELD { spec = (c, t),
							   synthetic = false,
							   offset } } =
		    pr_vdecl ("typ_f_" ^ name, rti_ty t)
		  | pr_field_rti _ = ()

		fun pr_field_acc0 (name, p, t) =
		    pr_vdecl (concat ["f_", name, p],
			      Arrow (Con ("su_obj" ^ p, [StUn tag, Type "'c"]),
				     t))

		fun pr_bf_acc (name, p, sg, c) =
		    pr_field_acc0 (name, p, Con (sg ^ "bf", [cro c]))

		fun pr_field_acc p { name, spec = S.OFIELD { spec = (c, t),
							     synthetic = false,
							     offset } } =
		    pr_field_acc0 (name, p, obj_ty p (t, cro c))
		  | pr_field_acc p { name, spec = S.OFIELD _ } = ()
		  | pr_field_acc p { name, spec = S.SBF bf } =
		    pr_bf_acc (name, p, "s", #constness bf)
		  | pr_field_acc p { name, spec = S.UBF bf } =
		    pr_bf_acc (name, p, "u", #constness bf)
	    in
		nl ();
		nl (); str (concat ["structure ", K, "_", tag,
				    " : sig (* ", su, " ", tag, " *)"]);
		Box 4;
		pr_tdef ("tag", StUn tag);
		nl ();
		nl (); str (concat ["(* size for this ", su, " *)"]);
		pr_vdecl ("size", Con ("S.size", [Con ("su", [StUn tag])]));
		nl ();
		nl (); str (concat ["(* RTI for this ", su, " *)"]);
		pr_vdecl ("typ", Con ("T.su_typ", [StUn tag]));
		nl ();
		nl (); str "(* witness types for fields *)";
		app pr_field_typ fields;
		nl ();
		nl (); str "(* RTI for fields *)";
		app pr_field_rti fields;
		nl ();
		nl (); str "(* field accessors *)";
		app (pr_field_acc "") fields;
		nl ();
		nl (); str "(* field accessors (lightweight variety) *)";
		app (pr_field_acc "'") fields;
		endBox ();
		nl (); str (concat ["end (* structure ", K, "_", tag, " *)"])
	    end

	    fun pr_struct_structure { tag, size, anon, fields } =
		pr_su_structure (St, "S", "struct", tag, fields)
	    fun pr_union_structure { tag, size, anon, largest, all } =
		pr_su_structure (Un, "U", "union", tag, all)

	    fun pr_gty_rti { name, spec } =
		pr_vdecl ("typ_t_" ^ name, rti_ty spec)

	    fun pr_gvar_obj { name, spec = (c, t) } =
		pr_vdecl ("g_" ^ name, Arrow (Unit, obj_ty "" (t, rwro c)))

	    fun pr_gfun_rti { name, spec } =
		pr_vdecl ("typ_fn_" ^ name, rti_ty (S.FPTR spec))

	    fun pr_gfun_fptr { name, spec } =
		pr_vdecl ("fptr_fn_" ^ name,
			  Arrow (Unit, wtn_ty (S.FPTR spec)))

	    fun pr_gfun_func p { name, spec } =
		pr_vdecl (concat ["fn_", name, p], topfunc_ty p spec)

	    fun pr_isu (K, tag) =
		(nl ();
		 str (concat ["structure ", istruct (K, tag),
			      " : POINTER_TO_INCOMPLETE_TYPE"]))
	    fun pr_istruct tag = pr_isu ("S", tag)
	    fun pr_iunion tag = pr_isu ("U", tag)
	in
	    (* Generating the signature file... *)
	    str dontedit;
	    nl (); str credits;
	    nl (); str commentsto;
	    nl (); str "local open C.Dim C in";
	    nl (); str (concat ["signature ", signame, " = sig"]);
	    VBox 4;
	    app pr_istruct incomplete_structs;
	    app pr_iunion incomplete_unions;
	    app pr_struct_tag structs;
	    app pr_union_tag unions;
	    app pr_struct_structure structs;
	    app pr_union_structure unions;
	    if not (List.null gtys) then
		(nl (); nl (); str "(* RTI for typedefs *)";
		 app pr_gty_rti gtys)
	    else ();
	    if not (List.null gvars) then
		(nl (); nl (); str "(* object handles for global variables *)";
		 app pr_gvar_obj gvars)
	    else ();
	    if not (List.null gfuns) then
		(nl (); nl (); str "(* RTI for global function(-pointer)s *)";
		 app pr_gfun_rti gfuns;
		 nl (); nl (); str "(* global function pointers *)";
		 app pr_gfun_fptr gfuns;
		 nl (); nl (); str "(* global functions *)";
		 app (pr_gfun_func "'") gfuns;
		 app (pr_gfun_func "") gfuns)
	    else ();
	    endBox ();
	    nl (); str (concat ["end (* signature ", signame, " *)"]);
	    nl (); str "end (* local *)";
	    nl ();

	    PP.closeStream sigpp
	end

	fun do_fct_file () = let
	    val strpp = openPP strfile

	    fun nl () = PP.newline strpp
	    fun str s = PP.string strpp s
	    fun sp () = PP.space strpp 1
	    fun nsp () = PP.nbSpace strpp 1
	    fun Box a = PP.openBox strpp (PP.Abs a)
	    fun HBox () = PP.openHBox strpp
	    fun HOVBox a = PP.openHOVBox strpp (PP.Abs a)
	    fun VBox a = PP.openVBox strpp (PP.Abs a)
	    fun endBox () = PP.closeBox strpp
	    fun ppty t = P.ppType strpp t
	    fun ppExp e = P.ppExp strpp e
	    fun ppFun x = P.ppFun strpp x

	    fun pr_fdef (f, args, res) = (nl (); ppFun (f, args, res))

	    fun pr_def_t (sep, keyword, connector) (v, t) =
		(sep ();
		 HOVBox 4; str keyword; nsp (); str v; nsp (); str connector;
		 sp (); ppty t; endBox ())

	    val pr_vdecl = pr_def_t (fn () => (), "val", ":")

	    val pr_tdef = pr_def_t (nl, "type", "=")

	    fun pr_vdef (v, e) =
		(nl ();
		 HOVBox 4; str "val"; nsp (); str v; nsp (); str "=";
		 sp (); ppExp e; endBox ())

	    fun pr_su_tag (su, tag, false) =
		let fun build [] = Type su
		      | build (h :: tl) = Con ("t_" ^ String.str h, [build tl])
		in
		    pr_tdef (concat [su, "_", tag],
			     build (rev (String.explode tag)))
		end
	      | pr_su_tag (su, tag, true) =
		(nl (); str "local";
		 VBox 4;
		 nl (); str
		    "structure X :> sig type t end = struct type t = unit end";
		 endBox ();
		 nl (); str "in";
		 VBox 4;
		 pr_tdef (concat [su, "_", tag],
			  Type "X.t");
		 endBox ();
		 nl (); str "end")

	    fun pr_struct_tag { tag, size, anon, fields } =
		pr_su_tag ("s", tag, anon)
	    fun pr_union_tag { tag, size, anon, largest, all } =
		pr_su_tag ("u", tag, anon)

	    fun pr_su_tag_copy (k, tag) = let
		val tn = concat [k, "_", tag]
	    in
		pr_tdef (tn, Type tn)
	    end

	    fun pr_struct_tag_copy { tag, size, anon, fields } =
		pr_su_tag_copy ("s", tag)
	    fun pr_union_tag_copy { tag, size, anon, largest, all } =
		pr_su_tag_copy ("u", tag)

	    fun pr_fptr_rti ({ args, res }, i) = let

		(* cproto encoding *)
		fun List t = Con ("list", [t])
		val Real = Type "real"
		val Char = Type "char"
		val Word8 = Type "Word8.word"
		val Int31 = Type "Int31.int"
		val Word31 = Type "Word31.word"
		val Int32 = Type "Int32.int"
		val Word32 = Type "Word32.word"
		val String = Type "string"
		val Exn = Type "exn"

		(* see src/compiler/Semant/types/cproto.sml for these... *)
		val E_double = Real
		val E_float = List Real
		val E_schar = Char
		val E_uchar = Word8
		val E_sint = Int31
		val E_uint = Word31
		val E_slong = Int32
		val E_ulong = Word32
		val E_sshort = List Char
		val E_ushort = List Word8
		val E_sllong = List Int32 (* not used yet *)
		val E_ullong = List Word32(* not used yet *)
		val E_ptr = String
		val E_nullstruct = Exn

		fun encode S.DOUBLE = E_double
		  | encode S.FLOAT = E_float
		  | encode S.SCHAR = E_schar
		  | encode S.UCHAR = E_uchar
		  | encode S.SINT = E_sint
		  | encode S.UINT = E_uint
		  | encode S.SSHORT = E_sshort
		  | encode S.USHORT = E_ushort
		  | encode S.SLONG = E_slong
		  | encode S.ULONG = E_ulong
		  | encode (S.PTR _ | S.VOIDPTR | S.FPTR _) = E_ptr
		  | encode (S.ARR _) = raise Fail "unexpected array"
		  | encode (S.STRUCT t) =
		    encode_fields (#fields (get_struct t))
		  | encode (S.UNION t) =
		    encode_fields [#largest (get_union t)]

		and encode_fields fields = let
		    fun f0 (S.ARR { t, d = 0, ... }, a) = a
		      | f0 (S.ARR { t, d = 1, ... }, a) = f0 (t, a)
		      | f0 (S.ARR { t, d, esz }, a) =
			f0 (t, f0 (S.ARR { t = t, d = d - 1, esz = esz }, a))
		      | f0 (t, a) = encode t :: a
		    fun f ({ spec = S.OFIELD { spec, ... }, name }, a) =
			f0 (#2 spec, a)
		      | f (_, a) = a
		    val fel = foldr f [] fields
		in
		    case fel of
			[] => E_nullstruct
		      | fel => Tuple (Unit :: fel)
		end

		val e_arg = Tuple (Unit :: map encode args)
		val e_res = case res of NONE => Unit | SOME t => encode t
		val e_proto = Con ("list", [Arrow (e_arg, e_res)])

		(* generating the call operation *)

		(* low-level type used to communicate a value to the
		 * low-level call operation *)
		fun mlty S.SCHAR = Type "CMemory.cc_schar"
		  | mlty S.UCHAR = Type "CMemory.cc_uchar"
		  | mlty S.SINT = Type "CMemory.cc_sint"
		  | mlty S.UINT = Type "CMemory.cc_uint"
		  | mlty S.SSHORT = Type "CMemory.cc_sshort"
		  | mlty S.USHORT = Type "CMemory.cc_ushort"
		  | mlty S.SLONG = Type "CMemory.cc_slong"
		  | mlty S.ULONG = Type "CMemory.cc_ulong"
		  | mlty S.FLOAT = Type "CMemory.cc_float"
		  | mlty S.DOUBLE = Type "CMemory.cc_double"
		  | mlty (S.VOIDPTR | S.PTR _ | S.FPTR _ |
			  S.STRUCT _) = Type "CMemory.cc_addr"
		  | mlty (S.ARR _ | S.UNION _) = raise Fail "unexpected type"

		fun wrap (e, n) =
		    EApp (EVar ("CMemory.wrap_" ^ n),
			  EApp (EVar ("Cvt.ml_" ^ n), e))

		fun vwrap e = EApp (EVar "CMemory.wrap_addr",
				    EApp (EVar "reveal", e))
		fun fwrap e = EApp (EVar "CMemory.wrap_addr",
				    EApp (EVar "freveal", e))
		fun pwrap e = EApp (EVar "CMemory.wrap_addr",
				    EApp (EVar "reveal",
					  EApp (EVar "Ptr.inject'", e)))
		fun iwrap (K, tag, e) =
		    EApp (EVar "CMemory.wrap_addr",
			  EApp (EVar "reveal",
				EApp (EVar (istruct (K, tag) ^ ".inject'"),
				      e)))

		fun suwrap e = pwrap (EApp (EVar "Ptr.|&!", e))

		(* this code is for passing structures in pieces
		 * (member-by-member); we don't use this and rather
		 * provide a pointer to the beginning of the struct *)

		fun arglist ([], _) = ([], [])
		  | arglist (h :: tl, i) = let
			val p = EVar ("x" ^ Int.toString i)
			val (ta, ea) = arglist (tl, i + 1)
			fun sel e = (mlty h :: ta, e :: ea)
		    in
			case h of
			    (S.STRUCT _ | S.UNION _) => sel (suwrap p)
			  | (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			     S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			     S.FLOAT | S.DOUBLE) => sel (wrap (p, stem h))
			  | S.VOIDPTR => sel (vwrap p)
			  | S.PTR (_, t) =>
			    (case incomplete t of
				 SOME (K, tag) => sel (iwrap (K, tag, p))
			       | NONE => sel (pwrap p))
			  | S.FPTR _ => sel (fwrap p)
			  | S.ARR _ => raise Fail "unexpected array argument"
		    end

		val (ml_res_t,
		     extra_arg_v, extra_arg_e, extra_ml_arg_t,
		     res_wrap) =
		    case res of
			NONE => (Unit, [], [], [], fn r => r)
		      | SOME (S.STRUCT _ | S.UNION _) =>
			(Unit,
			 [EVar "x0"],
			 [suwrap (EVar "x0")],
			 [Type "CMemory.cc_addr"],
			 fn r => ESeq (r, EVar "x0"))
		      | SOME t => let
			    fun unwrap n r =
				EApp (EVar ("Cvt.c_" ^ n),
				      EApp (EVar ("CMemory.unwrap_" ^ n), r))
			    fun punwrap cast r =
				EApp (EVar cast,
				      EApp (EVar "CMemory.unwrap_addr", r))
			    fun iunwrap (K, tag, t) r =
				EApp (EApp (EVar (istruct (K, tag) ^
						  ".project'"),
					    rti_val t),
				      punwrap "vcast" r)
			    val res_wrap =
				case t of
				    (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				     S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				     S.FLOAT | S.DOUBLE) => unwrap (stem t)
				  | S.VOIDPTR => punwrap "vcast"
				  | S.FPTR _ => punwrap "fcast"
				  | t0 as S.PTR (_, t) =>
				    (case incomplete t of
					 SOME (K, tag) => iunwrap (K, tag, t0)
				       | NONE => punwrap "pcast")
				  | (S.STRUCT _ | S.UNION _ | S.ARR _) =>
				    raise Fail "unexpected result type"
			in
			    (mlty t, [], [], [], res_wrap)
			end

		val (ml_args_tl, args_el) = arglist (args, 1)

		val ml_args_t = Tuple (extra_ml_arg_t @ ml_args_tl)

		val arg_vl =
		    rev (#1 (foldl (fn (_, (a, i)) =>
				       (EVar ("x" ^ Int.toString i) :: a,
					i + 1)) ([], 1)
				   args))

		val arg_e = ETuple (extra_arg_e @ args_el)
	    in
		nl ();
		str (concat ["val ", "fptr_rti_", Int.toString i, " = let"]);
		VBox 4;
		pr_vdef ("callop",
			  EConstr (EVar "RawMemInlineT.rawccall",
				   Arrow (Tuple [Type "Word32.word",
						 ml_args_t,
						 e_proto],
					  ml_res_t)));
		pr_fdef ("mkcall",
			 [EVar "a", ETuple (extra_arg_v @ arg_vl)],
			 res_wrap (EApp (EVar "callop",
					 ETuple [EVar "a", arg_e,
						 EVar "nil"])));
		endBox ();
		nl (); str "in";
		VBox 4;
		nl (); ppExp (EConstr (EApp (EVar "mk_fptr_typ",
					     EVar "mkcall"),
				       rti_ty (S.FPTR { args = args,
							res = res })));
		endBox ();
		nl (); str "end"
	    end

	    fun pr_su_structure (StUn, k, K, tag, size, fields) = let
		fun rwro S.RW = "rw"
		  | rwro S.RO = "ro"
		fun pr_field_typ { name, spec = S.OFIELD { spec = (c, t),
							   synthetic = false,
							   offset } } =
		    pr_tdef ("t_f_" ^ name, wtn_ty t)
		  | pr_field_typ _ = ()
		fun pr_field_rti { name, spec = S.OFIELD { spec = (c, t),
							   synthetic = false,
							   offset } } =
		    pr_vdef ("typ_f_" ^ name, rti_val t)
		  | pr_field_rti _ = ()

		fun pr_bf_acc (name, p, sign,
			       { offset, constness, bits, shift }) =
		    let val maker =
			    concat ["mk_", rwro constness, "_", sign, "bf", p]
		    in
			pr_fdef (concat ["f_", name, p],
				 [EVar "x"],
				 EApp (EApp (EVar maker,
					     ETuple [EInt offset,
						     EWord bits,
						     EWord shift]),
				       EVar "x"))
		    end

		fun pr_field_acc' { name, spec = S.OFIELD x } =
		    let val { synthetic, spec = (c, t), offset, ... } = x
		    in
			if synthetic then ()
			else pr_fdef (concat ["f_", name, "'"],
				      [EConstr (EVar "x",
						Suobj''c (StUn tag))],
				      EConstr (EApp (EApp (EVar "mk_field'",
							   EInt offset),
						     EVar "x"),
					       obj_ty "'" (t, cro c)))
		    end
		  | pr_field_acc' { name, spec = S.SBF bf } =
		    pr_bf_acc (name, "'", "s", bf)
		  | pr_field_acc' { name, spec = S.UBF bf } =
		    pr_bf_acc (name, "'", "u", bf)

		fun pr_field_acc { name, spec = S.OFIELD { offset,
							   spec = (c, t),
							   synthetic } } =
		    if synthetic then ()
		    else let
			    val maker = concat ["mk_", rwro c, "_field"]
			    val rtival = EVar ("typ_f_" ^ name)
			in
			    pr_fdef ("f_" ^ name,
				     [EVar "x"],
				     EApp (EApp (EApp (EVar maker, rtival),
						 EInt offset),
					   EVar "x"))
			end
		  | pr_field_acc { name, spec = S.SBF bf } =
		    pr_bf_acc (name, "", "s", bf)
		  | pr_field_acc { name, spec = S.UBF bf } =
		    pr_bf_acc (name, "", "u", bf)
	    in
		nl ();
		str (concat ["structure ", K, "_", tag, " = struct"]);
		Box 4;
		nl (); str (concat ["open ", K, "_", tag]);
		app pr_field_typ fields;
		app pr_field_rti fields;
		app pr_field_acc' fields;
		app pr_field_acc fields;
		endBox ();
		nl (); str "end"
	    end

	    fun pr_struct_structure { tag, size, anon, fields } =
		pr_su_structure (St, "s", "S", tag, size, fields)
	    fun pr_union_structure { tag, size, anon, largest, all } =
		pr_su_structure (Un, "u", "U", tag, size, all)

	    fun pr_gty_rti { name, spec } =
		pr_vdef ("typ_t_" ^ name, rti_val spec)

	    fun pr_addr (prefix, name) =
		pr_vdef (prefix ^ name,
			 EApp (EApp (EVar "D.lib_symbol", EVar "so_h"),
			       EString name))

	    fun pr_gvar_addr { name, spec } = pr_addr ("gh_", name)

	    fun pr_gvar_obj { name, spec = (c, t) } = let
		val rwobj = EApp (EApp (EVar "mk_obj", rti_val t),
				  EApp (EVar "D.addr", EVar ("gh_" ^ name)))
		val obj = case c of S.RW => rwobj
				  | S.RO => EApp (EVar "ro", rwobj)
	    in
		pr_fdef ("g_" ^ name, [ETuple []], obj)
	    end

	    fun pr_gfun_rti { name, spec } =
		pr_vdef ("typ_fn_" ^ name, rti_val (S.FPTR spec))

	    fun pr_gfun_addr { name, spec } = pr_addr ("fnh_", name)

	    fun pr_gfun_fptr { name, spec } =
		pr_fdef ("fptr_fn_" ^ name,
			 [ETuple []],
			 EApp (EApp (EVar "mk_fptr", EVar ("typ_fn_" ^ name)),
			       EApp (EVar "D.addr", EVar ("fnh_" ^ name))))

	    fun pr_gfun_func is_light { name, spec = { args, res } } = let
		val p = if is_light then "'" else ""
		val ml_vars =
		    rev (#1 (foldl (fn (_, (l, i)) =>
				       (EVar ("x" ^ Int.toString i) :: l,
					i + 1))
				   ([], 1)
				   args))
		fun app0 (what, e) =
		    if is_light then e else EApp (EVar what, e)
		fun light (what, e) = app0 ("Light." ^ what, e)
		fun heavy (what, t, e) =
		    if is_light then e
		    else EApp (EApp (EVar ("Heavy." ^ what), rti_val t), e)
		    
		fun oneArg (e, t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				     S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				     S.FLOAT | S.DOUBLE)) =
		    EApp (EVar ("Cvt.c_" ^ stem t), e)
		  | oneArg (e, (S.STRUCT _ | S.UNION _)) =
		    EApp (EVar "ro'", light ("obj", e))
		  | oneArg (e, S.PTR (_, t)) =
		    (case incomplete t of
			 SOME (K, tag) =>
			 app0 (istruct (K, tag) ^ ".light", e)
		       | NONE => light ("ptr", e))
		  | oneArg (e, S.FPTR _) = light ("fptr", e)
		  | oneArg (e, S.VOIDPTR) = e
		  | oneArg (e, S.ARR _) = raise Fail "array argument type"
		val c_exps = ListPair.map oneArg (ml_vars, args)
		val call = EApp (EVar "call",
				 ETuple [EApp (EVar ("fptr_fn_" ^ name),
					       ETuple []),
					 ETuple c_exps])
		val ml_res =
		    case res of
			SOME (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				    S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				    S.FLOAT | S.DOUBLE)) =>
			EApp (EVar ("Cvt.ml_" ^ stem t), call)
		      | SOME (t as (S.STRUCT _ | S.UNION _)) =>
			heavy ("obj", t, call)
		      | SOME (S.PTR (_, t)) =>
			(case incomplete t of
			     SOME (K, tag) =>
			     app0 (istruct (K, tag) ^ ".heavy", call)
			   | NONE => heavy ("ptr", t, call))
		      | SOME (t as S.FPTR _) => heavy ("fptr", t, call)
		      | SOME (S.ARR _) => raise Fail "array result type"
		      | (NONE | SOME S.VOIDPTR) => call
	    in
		pr_fdef (concat ["fn_", name, p], [ETuple ml_vars], ml_res)
	    end

	    fun pr_isu_arg (K, tag) =
		(sp (); str (concat ["structure ", istruct (K, tag),
				     " : POINTER_TO_INCOMPLETE_TYPE"]))
	    fun pr_istruct_arg tag = pr_isu_arg ("S", tag)
	    fun pr_iunion_arg tag = pr_isu_arg ("U", tag)

	    fun pr_isu_def (kw, K, tag) = let
		val n = istruct (K, tag)
	    in
		nl ();
		str (concat [kw, " ", n, " = ", n])
	    end
	    fun pr_istruct_res tag = pr_isu_def ("where", "S", tag)
	    fun pr_iunion_res tag = pr_isu_def ("where", "U", tag)
	    fun pr_istruct_def tag = pr_isu_def ("structure", "S", tag)
	    fun pr_iunion_def tag = pr_isu_def ("structure", "U", tag)

	    fun pr_pre_su (K, k, STUN, StUn, tag, size) =
		(nl (); str (concat ["structure ", K, "_", tag, " = struct"]);
		 VBox 4;
		 pr_tdef ("tag", Type (concat [k, "_", tag]));
		 pr_vdef ("size",
			  EConstr (EApp (EVar "C_Int.mk_su_size", EWord size),
				   Con ("C.S.size",
					[Con ("C.su", [StUn tag])])));
		 pr_vdef ("typ", EApp (EVar "C_Int.mk_su_typ", EVar "size"));
		 endBox ();
		 nl (); str "end")

	    fun pr_pre_struct { tag, size, anon, fields } =
		pr_pre_su ("S", "s", S.STRUCT, St, tag, size)
	    fun pr_pre_union { tag, size, anon, largest, all } =
		pr_pre_su ("U", "u", S.UNION, Un, tag, size)
	in
	    (* Generating the functor file... *)
	    str dontedit;
	    nl (); str credits;
	    nl (); str commentsto;
	    nl ();
	    str (concat ["structure ", strname, " = struct"]);
	    VBox 4;

	    if length structs + length unions <> 0 then
		(nl (); str "local";
		 VBox 4;
		 nl (); str "open Tag";
		 endBox ();
		 nl (); str "in";
		 VBox 4;
		 (* definitions for struct/union tags *)
		 app pr_struct_tag structs;
		 app pr_union_tag unions;
		 endBox ();
		 nl (); str "end")
	    else ();

	    (* "pre"-structures for all structures and unions *)
	    app pr_pre_struct structs;
	    app pr_pre_union unions;

	    (* the main functor *)
	    nl ();
	    str "functor"; nsp (); str (strname ^ "Fn");
	    HOVBox 4;
	    sp ();
	    PP.openHVBox strpp (PP.Rel 1);
	    str "(";
	    pr_vdecl ("library", Arrow (Unit, Type "DynLinkage.lib_handle"));
	    app pr_istruct_arg incomplete_structs;
	    app pr_iunion_arg incomplete_unions;
	    str ")";
	    endBox ();
	    sp (); str ":"; sp (); str signame;
	    VBox 4;
	    app pr_istruct_res incomplete_structs;
	    app pr_iunion_res incomplete_unions;
	    endBox ();
	    nsp (); str "=";
	    endBox ();
	    nl (); str "struct";
	    VBox 4;

	    (* copy definitions for struct/union tags *)
	    app pr_struct_tag_copy structs;
	    app pr_union_tag_copy unions;

	    (* other local stuff (to define RTI for function pointers) *)
	    nl (); str "local";
	    VBox 4;
	    nl (); str "structure D = DynLinkage";
	    nl (); str "open C.Dim C_Int";

	    (* low-level call operations for all function pointers *)
	    app pr_fptr_rti fptr_types;

	    (* grab the library handle *)
	    nl (); str "val so_h = library ()";
	    (* addr handles for global variables *)
	    app pr_gvar_addr gvars;
	    (* addr handles for global C functions *)
	    app pr_gfun_addr gfuns;

	    endBox ();
	    nl (); str "in";
	    VBox 4;
	    (* carry-throughs for incomplete types *)
	    app pr_istruct_def incomplete_structs;
	    app pr_iunion_def incomplete_unions;
	    (* ML structures corresponding to C struct declarations *)
	    app pr_struct_structure structs;
	    (* ML structurse corresponding to C union declarations *)
	    app pr_union_structure unions;

	    (* RTI for C typedefs *)
	    app pr_gty_rti gtys;
	    (* (suspended) objects for global variables *)
	    app pr_gvar_obj gvars;
	    (* RTI for function pointers corresponding to global C functions *)
	    app pr_gfun_rti gfuns;
	    (* (suspended) function pointers for global C functions *)
	    app pr_gfun_fptr gfuns;
	    (* ML functions corresponding to global C functions *)
	    app (pr_gfun_func true) gfuns;(* light *)
	    app (pr_gfun_func false) gfuns;(* heavy *)
	    endBox ();
	    nl (); str "end";		(* local *)
	    endBox ();
	    nl (); str "end";		(* functor/struct *)
	    endBox ();
	    nl (); str "end";		(* structure/struct *)
	    nl ();

	    PP.closeStream strpp
	end

	fun do_cm_file () = let
	    val cmpp = openPP cmfile

	    fun nl () = PP.newline cmpp
	    fun str s = PP.string cmpp s
	    fun sp () = PP.space cmpp 1
	    fun nsp () = PP.nbSpace cmpp 1
	    fun VBox a = PP.openVBox cmpp (PP.Abs a)
	    fun endBox () = PP.closeBox cmpp
	    fun line s = (nl (); str s)
	    val ls =
		case lambdasplit of
		    NONE => ""
		  | SOME s => concat ["\t(lambdasplit:", s, ")"]
	in
	    (* Generating the .cm file... *)
	    str dontedit;
	    line credits;
	    line commentsto;
	    line "(primitive c-int)";
	    line "library";
	    VBox 4;
	    line ("signature " ^ signame);
	    line ("structure " ^ strname);
	    endBox ();
	    line "is";
	    VBox 4;
	    app line ["$/basis.cm","$/c-int.cm", "$smlnj/init/init.cmi : cm"];
	    line (sigfile ^ ls);
	    line (strfile ^ ls);
	    endBox ();
	    nl ();

	    PP.closeStream cmpp
	end
    in
	do_sig_file ();
	do_fct_file ();
	do_cm_file ()
    end
end
end

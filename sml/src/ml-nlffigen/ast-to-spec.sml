(*
 * ast-to-spec.sml - Conversion from CKIT "ast" to a "spec" (see spec.sml).
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure AstToSpec = struct

    structure A = Ast
    structure B = Bindings

    fun bug m = raise Fail ("AstToSpec: bug: " ^ m)
    fun err m = raise Fail ("AstToSpec: error: " ^ m)
    fun warn m = TextIO.output (TextIO.stdErr, "AstToSpec: warning: " ^ m)

    fun build (bundle, sizes: Sizes.sizes, idlfile, allSU) = let
	val { ast, tidtab, errorCount, warningCount,
	      auxiliaryInfo = { aidtab, implicits, env } } = bundle

	fun isThisFile SourceMap.UNKNOWN = false
	  | isThisFile (SourceMap.LOC { srcFile, ... }) = srcFile = idlfile

	fun isPublicName "" = false
	  | isPublicName n = String.sub (n, 0) <> #"_"

	fun includedSU (tag, loc) =
	    (allSU orelse isThisFile loc) andalso isPublicName tag

	fun includedTy (n, loc) = isThisFile loc andalso isPublicName n

	fun isFunction t = TypeUtil.isFunction tidtab t
	fun getFunction t = TypeUtil.getFunction tidtab t
	fun getCoreType t = TypeUtil.getCoreType tidtab t

	fun constness t =
	    if TypeUtil.isConst tidtab t then Spec.RO
	    else case getCoreType t of
		     A.Array (_, t) => constness t
		   | _ => Spec.RW

	val sizerec = { sizes = sizes, err = err, warn = err, bug = bug }

	fun sizeOf t = #bytes (Sizeof.byteSizeOf sizerec tidtab t)

	val bytebits = #bits (#char sizes)
	val intbits = #bits (#int sizes)
	val intalign = #align (#int sizes)

	fun getField (m, l) = Sizeof.getField sizerec (m, l)

	fun fieldOffsets t =
	    case Sizeof.fieldOffsets sizerec tidtab t of
		NONE => bug "no field offsets"
	      | SOME l => l

	val structs = ref []
	val unions = ref []
	val gtys = ref []
	val gvars = ref []
	val gfuns = ref []

	val seen_structs = ref []
	val seen_unions = ref []

	val nexttag = ref 0
	val tags = Tidtab.uidtab () : string Tidtab.uidtab

	fun tagname (NONE, tid) =
	    (case Tidtab.find (tags, tid) of
		 SOME s => s
	       | NONE => let
		     val i = !nexttag
		     val s = Int.toString i
		 in
		     nexttag := i + 1;
		     Tidtab.insert (tags, tid, s);
		     s
		 end)
	  | tagname (SOME n, _) = n

	fun valty A.Void = err "void variable type"
	  | valty A.Ellipses = err "ellipses variable type"
	  | valty (A.Qual (q, t)) = valty t
	  | valty (A.Numeric (_, _, A.SIGNED, A.CHAR, _)) = Spec.SCHAR
	  | valty (A.Numeric (_, _, A.UNSIGNED, A.CHAR, _)) = Spec.UCHAR
	  | valty (A.Numeric (_, _, A.SIGNED, A.INT, _)) = Spec.SINT
	  | valty (A.Numeric (_, _, A.UNSIGNED, A.INT, _)) = Spec.UINT
	  | valty (A.Numeric (_, _, A.SIGNED, A.SHORT, _)) = Spec.SSHORT
	  | valty (A.Numeric (_, _, A.UNSIGNED, A.SHORT, _)) = Spec.USHORT
	  | valty (A.Numeric (_, _, A.SIGNED, A.LONG, _)) = Spec.SLONG
	  | valty (A.Numeric (_, _, A.UNSIGNED, A.LONG, _)) = Spec.ULONG
	  | valty (A.Numeric (_, _, _, A.FLOAT, _)) = Spec.FLOAT
	  | valty (A.Numeric (_, _, _, A.DOUBLE, _)) = Spec.DOUBLE
	  | valty (A.Numeric _) = bug "numeric type not (yet) supported"
	  | valty (A.Array (NONE, t)) = valty (A.Pointer t)
	  | valty (A.Array (SOME (n, _), t)) =
	    Spec.ARR { t = valty t, d = Int.fromLarge n, esz = sizeOf t }
	  | valty (A.Pointer t) =
	    (case getCoreType t of
		 A.Void => Spec.VOIDPTR
	       | A.Function f => fptrty f
	       | _ => Spec.PTR (cobj t))
	  | valty (A.Function f) = fptrty f
	  | valty (A.StructRef tid) = typeref (tid, Spec.STRUCT)
	  | valty (A.UnionRef tid) = typeref (tid, Spec.UNION)
	  | valty (A.EnumRef tid) = typeref (tid, (* hack *) fn _ => Spec.SINT)
	  | valty (A.TypeRef tid) =
	    typeref (tid, fn _ => bug "missing typedef info")
	  | valty A.Error = err "Error type"

	and typeref (tid, otherwise) =
	    case Tidtab.find (tidtab, tid) of
		NONE => bug "tid not bound in tidtab"
	      | SOME { name = SOME n, ntype = NONE, ... } => otherwise n
	      | SOME { name = NONE, ntype = NONE, ... } =>
		bug "both name and ntype missing in tidtab binding"
	      | SOME { name, ntype = SOME nct, location, ... } =>
		(case nct of
		     B.Struct (tid, members) =>
		     structty (tid, name, members, location)
		   | B.Union (tid, members) =>
		     unionty (tid, name, members, location)
		   | B.Enum (tid, _) => Spec.SINT (* for now (hack) *)
		   | B.Typedef (_, t) => let
			 val res = valty t
			 val n = 
			     case name of
				 NONE => bug "missing name in typedef"
			       | SOME n => n
			 fun sameName { name, spec } = name = n
		     in
			 if includedTy (n, location) then
			     case List.find sameName (!gtys) of
				 SOME _ => ()
			       | NONE =>
				 gtys := { name = n, spec = res } :: !gtys
			 else ();
			 res
		     end)

	and structty (tid, name, members, location) = let
	    val tag = tagname (name, tid)
	    val ty = Spec.STRUCT tag
	in
	    case List.find (fn tag' => tag = tag') (!seen_structs) of
		SOME _ => ()
	      | NONE =>
		if includedSU (tag, location) then
		    let val _ = seen_structs := tag :: !seen_structs

			val fol = fieldOffsets (A.StructRef tid)
			val ssize = sizeOf (A.StructRef tid)

			fun bfspec (offset, bits, shift, (c, t)) = let
			    val offset = offset
			    val bits = Word.fromLargeInt bits
			    val shift = Endian.shift (shift, intbits, bits)
			    val r = { offset = offset,
				      constness = c,
				      bits = bits,
				      shift = shift }
			in
			    case t of
				Spec.UINT => Spec.UBF r
			      | Spec.SINT => Spec.SBF r
			      | _ => err "non-int bitfield"
			end

			fun synthetic (synth, (_, false), _) = ([], synth)
			  | synthetic (synth, (endp, true), startp) =
			    if endp = startp then ([], synth)
			    else ([{ name = Int.toString synth,
				     spec = Spec.OFIELD
					{ offset = endp,
					  spec = (Spec.RW,
						  Spec.ARR { t = Spec.UCHAR,
							     d = startp - endp,
							     esz = 1 }),
					  synthetic = true } }],
				  synth+1)

			fun build ([], synth, gap) =
			    #1 (synthetic (synth, gap, ssize))
			  | build ((t, SOME m, NONE) :: rest, synth, gap) =
			    let val bitoff = #bitOffset (getField (m, fol))
				val bytoff = bitoff div bytebits
				val (filler, synth) =
				    synthetic (synth, gap, bytoff)
				val endp = bytoff + sizeOf t
			    in
				if bitoff mod bytebits <> 0 then
				    bug "non-bitfield not on byte boundary"
				else
				    filler @
				    { name = Symbol.name (#name m),
				      spec = Spec.OFIELD
						 { offset = bytoff,
						   spec = cobj t,
						   synthetic = false } } ::
				    build (rest, synth, (endp, false))
			    end
			  | build ((t, SOME m, SOME b) :: rest, synth, gap) =
			    let val bitoff = #bitOffset (getField (m, fol))
				val bytoff =
				    (intalign * (bitoff div intalign))
				    div bytebits
				val gap = (#1 gap, true)
			    in
				{ name = Symbol.name (#name m),
				  spec = bfspec (bytoff, b,
						 bitoff mod intalign,
						 cobj t) } ::
				build (rest, synth, gap)
			    end
			  | build ((t, NONE, SOME _) :: rest, synth, gap) =
			    build (rest, synth, (#1 gap, true))
			  | build ((_, NONE, NONE) :: _, _, _) =
			    bug "unnamed struct member (not bitfield)"

			val fields = build (members, 0, (0, false))
		    in
			structs := { tag = tag, 
				     anon = not (isSome name),
				     size = Word.fromInt ssize,
				     fields = fields } :: !structs
		    end
		else ();
	    ty
	end

	and unionty (tid, name, members, location) = let
	    val tag = tagname (name, tid)
	    val ty = Spec.UNION tag
	    val lsz = ref 0
	    val lg = ref { name = "",
			   spec = Spec.OFIELD { offset = 0,
						spec = (Spec.RW, Spec.SINT),
						synthetic = true } }
	    fun mkField (t, m: A.member) = let
		val sz = sizeOf t
		val e = { name = Symbol.name (#name m),
			  spec = Spec.OFIELD { offset = 0,
					       spec = cobj t,
					       synthetic = false } }
	    in
		if sz > !lsz then (lsz := sz; lg := e) else ();
		e
	    end
	in
	    case List.find (fn tag' => tag = tag') (!seen_unions) of
		SOME _ => ()
	      | NONE =>
		if includedSU (tag, location) then
		    let val _ = seen_unions := tag :: !seen_unions
			val all = map mkField members
		    in
			unions := { tag = tag,
				    anon = not (isSome name),
				    size = Word.fromInt
					       (sizeOf (A.UnionRef tid)),
				    largest = !lg,
				    all = all } :: !unions
		    end
		else ();
	    ty
	end

	and cobj t = (constness t, valty t)

	and fptrty f = Spec.FPTR (cft f)

	and cft (res, args) =
	    { res = case getCoreType res of
			A.Void => NONE
		      | _ => SOME (valty res),
	      args = case args of
			 [arg] => (case getCoreType arg of
				       A.Void => []
				     | _ => [valty arg])
		       | _ => map valty args }

	fun functionName (f: A.id) = let
	    val n = Symbol.name (#name f)
	in
	    if n = "_init" orelse n = "_fini" orelse
	       List.exists (fn { name, ... } => name = n) (!gfuns) then ()
	    else case #stClass f of
		     (A.EXTERN | A.DEFAULT) =>
		     (case getFunction (#ctype f) of
			  SOME fs =>
			  gfuns := { name = n, spec = cft fs } :: !gfuns
			| NONE => bug "function without function type")
		   | (A.AUTO | A.REGISTER | A.STATIC) => ()
	end

	fun varDecl (v: A.id) =
	    case #stClass v of
		(A.EXTERN | A.DEFAULT) =>
		if isFunction (#ctype v) then
		    functionName v
		else let val n = Symbol.name (#name v)
		     in
			 if List.exists
				(fn { name, ... } => name = n)
				(!gvars) then ()
			 else
			     gvars := { name = n,
					spec = cobj (#ctype v) } :: !gvars
		     end
	      | (A.AUTO | A.REGISTER | A.STATIC) => ()

	fun declaration (A.TypeDecl { tid, ... }) =
	    (* Spec.SINT is an arbitrary choice; the value gets
	     * ignored anyway *)
	    ignore (typeref (tid, fn _ => Spec.SINT))
	  | declaration (A.VarDecl (v, _)) = varDecl v

	fun coreExternalDecl (A.ExternalDecl d) = declaration d
	  | coreExternalDecl (A.FunctionDef (f, _, _)) = functionName f
	  | coreExternalDecl (A.ExternalDeclExt _) = ()

	fun externalDecl (A.DECL (d, _, l)) =
	    if isThisFile l then coreExternalDecl d else ()

	fun doast l = app externalDecl l
    in
	doast ast;
	{ structs = !structs,
	  unions = !unions,
	  gtys = !gtys,
	  gvars = !gvars,
	  gfuns = !gfuns }
    end
end

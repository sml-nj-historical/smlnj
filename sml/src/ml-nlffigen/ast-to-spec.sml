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

    structure SM = RedBlackMapFn (type ord_key = string
				  val compare = String.compare)

    exception VoidType
    exception Ellipsis

    fun bug m = raise Fail ("AstToSpec: bug: " ^ m)
    fun err m = raise Fail ("AstToSpec: error: " ^ m)
    fun warn m = TextIO.output (TextIO.stdErr, "AstToSpec: warning: " ^ m)

    fun build (bundle, sizes: Sizes.sizes, cfiles, match, allSU, eshift) = let

	val curLoc = ref "?"

	fun warnLoc m = warn (concat [!curLoc, ": ", m])

	val { ast, tidtab, errorCount, warningCount,
	      auxiliaryInfo = { aidtab, implicits, env } } = bundle

	fun realFunctionDefComing sy = let
	    fun isTheDef (A.DECL (A.FunctionDef (id, _, _), _, _)) =
		Symbol.equal (#name id, sy)
	      | isTheDef _ = false
	in
	    List.exists isTheDef ast
	end

	val srcOf = SourceMap.locToString

	fun isThisFile SourceMap.UNKNOWN = false
	  | isThisFile (SourceMap.LOC { srcFile, ... }) =
	    List.exists (fn f => f = srcFile) cfiles orelse
	    match srcFile

(*
	fun isPublicName "" = false
	  | isPublicName n = String.sub (n, 0) <> #"_"
*)

	fun includedSU (tag, loc) =
	    (allSU orelse isThisFile loc) (* andalso isPublicName tag *)

	fun includedTy (n, loc) = isThisFile loc (* andalso isPublicName n *)

	fun isFunction t = TypeUtil.isFunction tidtab t
	fun getFunction t = TypeUtil.getFunction tidtab t
	fun getCoreType t = TypeUtil.getCoreType tidtab t

	fun constness t =
	    if TypeUtil.isConst tidtab t then Spec.RO
	    else case getCoreType t of
		     A.Array (_, t) => constness t
		   | _ => Spec.RW

	val sizerec = { sizes = sizes, err = err, warn = warn, bug = bug }

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
	val enums = ref SM.empty

	val seen_structs = ref []
	val seen_unions = ref []

	val nexttag = ref 0
	val tags = Tidtab.uidtab () : string Tidtab.uidtab

	fun tagname (NONE, NONE, tid) =
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
	  | tagname (NONE, SOME n, _) = "_" ^ n
	  | tagname (SOME n, _, _) =
	    if String.sub (n, 0) = #"_" then "_" ^ n else n

	fun valty A.Void = raise VoidType
	  | valty A.Ellipses = raise Ellipsis
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
	    let val d = Int.fromLarge n
	    in
		if d < 0 then err "negative dimension"
		else Spec.ARR { t = valty t, d = d, esz = sizeOf t }
	    end
	  | valty (A.Pointer t) =
	    (case getCoreType t of
		 A.Void => Spec.VOIDPTR
	       | A.Function f => fptrty f
	       | _ => Spec.PTR (cobj t))
	  | valty (A.Function f) = fptrty f
	  | valty (A.StructRef tid) = typeref (tid, Spec.STRUCT, NONE)
	  | valty (A.UnionRef tid) = typeref (tid, Spec.UNION, NONE)
	  | valty (A.EnumRef tid) =
	    typeref (tid, (* hack *) fn _ => Spec.SINT, NONE)
	  | valty (A.TypeRef tid) =
	    typeref (tid, fn _ => bug "missing typedef info", NONE)
	  | valty A.Error = err "Error type"

	and valty_nonvoid t = valty t
	    handle VoidType => err "void variable type"


	and valty_td (A.StructRef tid, tdname) =
	    typeref (tid, Spec.STRUCT, tdname)
	  | valty_td (A.UnionRef tid, tdname) =
	    typeref (tid, Spec.UNION, tdname)
	  | valty_td (A.EnumRef tid, tdname) =
	    typeref (tid, fn _ => Spec.SINT, tdname)
	  | valty_td (t, _) = valty t

	and typeref (tid, otherwise, tdname) =
	    case Tidtab.find (tidtab, tid) of
		NONE => bug "tid not bound in tidtab"
	      | SOME { name = SOME n, ntype = NONE, ... } => otherwise n
	      | SOME { name = NONE, ntype = NONE, ... } =>
		bug "both name and ntype missing in tidtab binding"
	      | SOME { name, ntype = SOME nct, location, ... } =>
		(case nct of
		     B.Struct (tid, members) =>
		     structty (tid, name, tdname, members, location)
		   | B.Union (tid, members) =>
		     unionty (tid, name, tdname, members, location)
		   | B.Enum (tid, edefs) => let
			 fun one ({ name, uid, location, ctype, kind }, i) =
			     { name = Symbol.name name, spec = i }
			 val all = map one edefs
			 val tn = tagname (name, tdname, tid)
		     in
			 enums := SM.insert (!enums, tn,
					     { src = srcOf location,
					       tag = tn,
					       spec = all });
			 Spec.SINT
		     end
		   | B.Typedef (_, t) => let
			 val n = 
			     case name of
				 NONE => bug "missing name in typedef"
			       | SOME n => n
			 val res = valty_td (t, SOME n)
			 fun sameName { src, name, spec } = name = n
		     in
			 if includedTy (n, location) then
			     case List.find sameName (!gtys) of
				 SOME _ => ()
			       | NONE =>
				 gtys := { src = srcOf location,
					   name = n, spec = res } :: !gtys
			 else ();
			 res
		     end)

	and structty (tid, name, tdname, members, location) = let
	    val tag = tagname (name, tdname, tid)
	    val ty = Spec.STRUCT tag
	in
	    case List.find (fn tag' => tag = tag') (!seen_structs) of
		SOME _ => ()
	      | NONE => let
		    val _ = seen_structs := tag :: !seen_structs

		    val fol = fieldOffsets (A.StructRef tid)
		    val ssize = sizeOf (A.StructRef tid)

		    fun bfspec (offset, bits, shift, (c, t)) = let
			val offset = offset
			val bits = Word.fromLargeInt bits
			val shift = eshift (shift, intbits, bits)
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
		    structs := { src = srcOf location,
				 tag = tag, 
				 anon = not (isSome name),
				 size = Word.fromInt ssize,
				 exclude = not (includedSU (tag, location)),
				 fields = fields } :: !structs
		end;
	    ty
	end

	and unionty (tid, name, tdname, members, location) = let
	    val tag = tagname (name, tdname, tid)
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
	      | NONE => let
		    val _ = seen_unions := tag :: !seen_unions
		    val all = map mkField members
		in
		    unions := { src = srcOf location,
				tag = tag,
				anon = not (isSome name),
				size = Word.fromInt (sizeOf (A.UnionRef tid)),
				largest = !lg,
				exclude = not (includedSU (tag, location)),
				all = all } :: !unions
		end;
	    ty
	end

	and cobj t = (constness t, valty_nonvoid t)

	and fptrty f = Spec.FPTR (cft f)

	and cft (res, args) =
	    { res = case getCoreType res of
			A.Void => NONE
		      | _ => SOME (valty_nonvoid res),
	      args = case args of
			 [(arg, _)] => (case getCoreType arg of
				       A.Void => []
				     | _ => [valty_nonvoid arg])
		       | _ => let fun build [] = []
				    | build [(x, _)] =
				      ([valty_nonvoid x]
				       handle Ellipsis =>
					      (warnLoc
						   ("varargs not supported; \
						    \ignoring the ellipsis\n");
						   []))
				    | build ((x, _) :: xs) =
				      valty_nonvoid x :: build xs
			      in
				  build args
			      end }

	fun ft_argnames (res, args) =
	    let val optids = map (fn (_, optid) => optid) args
	    in
		if List.exists (not o isSome) optids then NONE
		else SOME (map valOf optids)
	    end

	fun functionName (f: A.id, ailo: A.id list option) = let
	    val n = Symbol.name (#name f)
	    val anlo = Option.map (map (Symbol.name o #name)) ailo
	in
	    if n = "_init" orelse n = "_fini" orelse
	       List.exists (fn { name, ... } => name = n) (!gfuns) then ()
	    else case #stClass f of
		     (A.EXTERN | A.DEFAULT) =>
		     (case getFunction (#ctype f) of
			  SOME fs =>
			  gfuns := { src = !curLoc,
				     name = n, spec = cft fs, argnames = anlo }
				   :: !gfuns
			| NONE => bug "function without function type")
		   | (A.AUTO | A.REGISTER | A.STATIC) => ()
	end

	fun varDecl (v: A.id) =
	    case #stClass v of
		(A.EXTERN | A.DEFAULT) =>
		(case getFunction (#ctype v) of
		     SOME fs => if realFunctionDefComing (#name v) then ()
				else functionName (v, ft_argnames fs)
		   | NONE =>
		     let val n = Symbol.name (#name v)
		     in
			 if List.exists
				(fn { name, ... } => name = n)
				(!gvars) then ()
			 else
			     gvars := { src = !curLoc, name = n,
					spec = cobj (#ctype v) } :: !gvars
		     end)
	      | (A.AUTO | A.REGISTER | A.STATIC) => ()

	fun declaration (A.TypeDecl { tid, ... }) =
	    (* Spec.SINT is an arbitrary choice; the value gets
	     * ignored anyway *)
	    (ignore (typeref (tid, fn _ => Spec.SINT, NONE))
	     handle VoidType => ())	(* ignore type aliases for void *)
	  | declaration (A.VarDecl (v, _)) = varDecl v

	fun coreExternalDecl (A.ExternalDecl d) = declaration d
	  | coreExternalDecl (A.FunctionDef (f, argids, _)) =
	    functionName (f, SOME argids)
	  | coreExternalDecl (A.ExternalDeclExt _) = ()

	fun externalDecl (A.DECL (d, _, l)) =
	    if isThisFile l then (curLoc := SourceMap.locToString l;
				  coreExternalDecl d)
	    else ()

	fun doast l = app externalDecl l
    in
	doast ast;
	{ structs = !structs,
	  unions = !unions,
	  gtys = !gtys,
	  gvars = !gvars,
	  gfuns = !gfuns,
	  enums = SM.listItems (!enums) } : Spec.spec
    end
end

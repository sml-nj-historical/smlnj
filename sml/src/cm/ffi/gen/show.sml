local
    type tt = Bindings.tidBinding Tidtab.uidtab
in
structure ShowC : sig
    val show : { ast: Ast.ast, tidtab: tt,
		 isMine: SourceMap.location -> bool }
	       -> string list
end = struct
    fun show { ast, isMine, tidtab : tt } = let

	(* reverse the ast and grab only stuff that is ours *)
	val cedl = let
	    fun loop ([], a) = a
	      | loop (Ast.DECL (ced, _, l) :: r, a) =
		if isMine l then loop (r, ced :: a) else loop (r, a)
	in
	    loop (ast, [])
	end

	(* this imperative stuff is ugly, but I have to live with
	 * what ckit gives me... *)
	val worklist = ref []
	val names = Tidtab.uidtab ()

	fun schedule t =
	    case Tidtab.find (names, t) of
		SOME _ => ()
	      | NONE =>
		(case Tidtab.find (tidtab, t) of
		     SOME { name = SOME n, ntype = SOME ty, ... } =>
		     (worklist := (t, n, ty) :: !worklist;
		      Tidtab.insert (names, t, n))
		   | _ => raise Fail "schedule:tidtab")

	fun ctype (t, r) = raise Fail "notyet"
	fun xfun x (v: Ast.id, r) =
	    x :: " " :: Symbol.name (#name v) :: ": " ::
	    ctype (#ctype v, ";\n" :: r)
	val nofun = xfun "var"
	val yesfun = xfun "fun"
	fun fdef (f, r) = yesfun (f, r)
	fun vdecl (v: Ast.id, r) =
	    case #stClass v of
		(Ast.EXTERN | Ast.DEFAULT) =>
		(case #kind v of
		     Ast.NONFUN => nofun (v, r)
		   | Ast.FUNCTION _ => yesfun (v, r))
	      | _ => r
	fun tdecl (t, r) = (schedule t; r)

	fun decl (Ast.TypeDecl { tid, ... }, r) = tdecl (tid, r)
	  | decl (Ast.VarDecl (v, _), r) = vdecl (v, r)

	fun ced (Ast.ExternalDecl d, r) = decl (d, r)
	  | ced (Ast.FunctionDef (f, _, _), r) = fdef (f, r)
	  | ced (Ast.ExternalDeclExt _, r) = r

	val r0 = foldl ced [] cedl
    in
	raise Fail "notyet"
    end
end
end

(*
 * Convert ASTs to CM's trimmed version thereof.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SKELCVT = sig
    val convert : { tree: GenericVC.Ast.dec,
		    err: GenericVC.ErrorMsg.severity ->
		         GenericVC.Ast.region -> string -> unit }
	-> { skeleton : Skeleton.decl, complain : unit -> unit }
end

structure SkelCvt :> SKELCVT = struct

    open GenericVC.Ast Skeleton

    structure S = Symbol
    structure SP = GenericVC.SymPath
    structure SS = SymbolSet
    structure EM = GenericVC.ErrorMsg

    type symbol = Symbol.symbol
    type path = symbol list

    infix o'
    fun (f o' g) (x, y) = f (g x, y)

    (* given a path, add an element to a set of module references *)
    fun s_addP ([], a) = a		(* can this happen at all? *)
      | s_addP ([only], a) = a		(* no module name here *)
      | s_addP (head :: _, a) = SS.add (a, head)

    (* given a path, add an element to a decl list *)
    fun d_addP ([], a) = a
      | d_addP (h :: _, []) = [Ref (SS.singleton h)]
      | d_addP (h :: _, Ref s :: a) = Ref (SS.add (s, h)) :: a
      | d_addP (h :: _, a) = Ref (SS.singleton h) :: a

    (* given a set of module references, add it to a decl list *)
    fun d_addS (s, a) =
	if SS.isEmpty s then a
	else case a of
	    [] => [Ref s]
	  | Ref s' :: a => Ref (SS.union (s, s')) :: a
	  | a => Ref s :: a

    fun localDec ([], [], a) = a
      | localDec ([], [Ref s], a) = d_addS (s, a)
      | localDec (Ref s :: t, body, a) = d_addS (s, localDec (t, body, a))
      | localDec (bdg, body, a) = Local (Seq bdg, Seq body) :: a

    fun con (s1, NONE) = s1
      | con (s1, SOME s2) = Con (s1, s2)

    fun c_dec ast = Seq (do_dec (ast, []))

    and do_dec (ValDec (l, _), a) = foldr c_vb a l
      | do_dec (ValrecDec (l, _), a) = foldr c_rvb a l
      | do_dec (FunDec (l, _), a) = foldr c_fb a l
      | do_dec (TypeDec l, a) = d_addS (foldl c_tb SS.empty l, a)
      | do_dec (DatatypeDec { datatycs, withtycs }, a) =
	d_addS (foldl c_db (foldl c_tb SS.empty withtycs) datatycs, a)
      | do_dec (AbstypeDec { abstycs, withtycs, body }, a) =
	(* body is syntactically restricted to ldecs,
	 * no module scoping here *)
	d_addS (foldl c_db (foldl c_tb SS.empty withtycs) abstycs,
		c_dec body :: a)
      | do_dec (ExceptionDec l, a) = d_addS (foldl c_eb SS.empty l, a)
      | do_dec ((StrDec l | AbsDec l), a) = Par (foldr c_strb [] l) :: a
      | do_dec (FctDec l, a) = Par (foldr c_fctb [] l) :: a
      | do_dec (SigDec l, a) = Par (foldr c_sigb [] l) :: a
      | do_dec (FsigDec l, a) = Par (foldr c_fsigb [] l) :: a
      | do_dec (LocalDec (bdg, body), a) =
	localDec (do_dec (bdg, []), do_dec (body, []), a)
      | do_dec (SeqDec l, a) = foldr do_dec a l
      | do_dec (OpenDec l, a) = Par (map (Open o Var o SP.SPATH) l) :: a
      | do_dec ((OvldDec _ | FixDec _), a) = a
      | do_dec (MarkDec (arg, _), a) = do_dec (arg, a)

    and c_strb (Strb { name, def, constraint }, a) =
	Bind (name, con (c_strexp def, sigexpConst constraint)) :: a
      | c_strb (MarkStrb (arg, _), a) = c_strb (arg, a)

    and c_fctb (Fctb { name, def }, a) = Bind (name, c_fctexp def) :: a
      | c_fctb (MarkFctb (arg, _), a) = c_fctb (arg, a)

    and c_sigb (Sigb { name, def }, a) = Bind (name, c_sigexp def) :: a
      | c_sigb (MarkSigb (arg, _), a) = c_sigb (arg, a)

    and c_fsigb (Fsigb { name, def }, a) = Bind (name, c_fsigexp def) :: a
      | c_fsigb (MarkFsigb (arg, _), a) = c_fsigb (arg, a)

    and c_strexp (VarStr path) = Var (SP.SPATH path)
      | c_strexp (BaseStr dec) = Decl (c_dec dec)
      | c_strexp (ConstrainedStr (s, NoSig)) = c_strexp s
      | c_strexp (ConstrainedStr (s, (Transparent g | Opaque g))) =
	Con (c_strexp s, c_sigexp g)
      | c_strexp (AppStr (p, l) | AppStrI (p, l)) =
	App (SP.SPATH p, map (c_strexp o #1) l)
      | c_strexp (LetStr (bdg, body)) = Let (c_dec bdg, c_strexp body)
      | c_strexp (MarkStr (s, _)) = c_strexp s

    and c_fctexp (VarFct (p, c)) = con (Var (SP.SPATH p), fsigexpConst c)
      | c_fctexp (BaseFct { params = p, body = b, constraint = c }) =
	Let (Seq (map functorParams p), con (c_strexp b, sigexpConst c))
      | c_fctexp (AppFct (p, l, c)) =
	con (App (SP.SPATH p, map (c_strexp o #1) l), fsigexpConst c)
      | c_fctexp (LetFct (bdg, body)) = Let (c_dec bdg, c_fctexp body)
      | c_fctexp (MarkFct (arg, _)) = c_fctexp arg

    and functorParams (NONE, c) = Open (c_sigexp c)
      | functorParams (SOME s, c) = Bind (s, c_sigexp c)

    and sigexpConst NoSig = NONE
      | sigexpConst (Transparent g | Opaque g) = SOME (c_sigexp g)

    and c_sigexp (VarSig s) = Var (SP.SPATH [s])
      | c_sigexp (AugSig (g, whspecs)) = let
	    fun f (WhType (_, _, ty), x) = c_ty (ty, x)
	      | f (WhStruct (_, head :: _), x) = SS.add (x, head)
	      | f _ = raise Fail "skel-cvt/c_sigexp"
	in
	    Let (Ref (foldl f SS.empty whspecs), c_sigexp g)
	end
      | c_sigexp (BaseSig l) = Decl (Seq (foldr c_spec [] l))
      | c_sigexp (MarkSig (arg, _)) = c_sigexp arg

    and fsigexpConst NoSig = NONE
      | fsigexpConst (Transparent fg | Opaque fg) = SOME (c_fsigexp fg)

    and c_fsigexp (VarFsig s) = Var (SP.SPATH [s])
      | c_fsigexp (BaseFsig { param, result }) =
	Let (Seq (map functorParams param), c_sigexp result)
      | c_fsigexp (MarkFsig (arg, _)) = c_fsigexp arg

    and c_spec (StrSpec l, a) = let
	    fun f (s, g, c) =
	        Bind (s, con (c_sigexp g, Option.map (Var o SP.SPATH) c))
        in
	    (Par (map f l)) :: a
        end
      | c_spec (TycSpec (l, _), a) = let
	    fun f ((_, _, SOME t), s) = c_ty (t, s)
	      | f (_, s) = s
	in
	    d_addS (foldl f SS.empty l, a)
	end
      | c_spec (FctSpec l, a) =
	Par (map (fn (s, g) => Bind (s, c_fsigexp g)) l) :: a
      | c_spec (ValSpec l, a) = d_addS (foldl (c_ty o' #2) SS.empty l, a)
      | c_spec (DataSpec { datatycs, withtycs }, a) =
	d_addS (foldl c_db (foldl c_tb SS.empty withtycs) datatycs, a)
      | c_spec (ExceSpec l, a) = d_addS (foldl (tyoption o' #2) SS.empty l, a)
      | c_spec (ShareStrSpec l, a) = foldl d_addP a l
      | c_spec (ShareTycSpec l, a) = d_addS (foldl s_addP SS.empty l, a)
      | c_spec (IncludeSpec g, a) = Open (c_sigexp g) :: a
      | c_spec (MarkSpec (arg, _), a) = c_spec (arg, a)

    and c_vb (Vb { pat, exp, lazyp }, a) =
	d_addS (c_pat (pat, SS.empty), c_exp (exp, a))
      | c_vb (MarkVb (arg, _), a) = c_vb (arg, a)

    and c_rvb (Rvb { var, exp, resultty, ... }, a) =
	d_addS (tyoption (resultty, SS.empty), c_exp (exp, a))
      | c_rvb (MarkRvb (arg, _), a) = c_rvb (arg, a)

    and c_fb (Fb (l, _), a) = foldr c_clause a l
      | c_fb (MarkFb (arg, _), a) = c_fb (arg, a)

    and c_clause (Clause { pats = p, resultty = t, exp = e }, a) =
	d_addS (foldl (c_pat o' #item) (tyoption (t, SS.empty)) p,
		c_exp (e, a))

    and c_tb (Tb { tyc, def, tyvars }, a) = c_ty (def, a)
      | c_tb (MarkTb (arg, _), a) = c_tb (arg, a)

    and c_db (Db { tyc, tyvars, rhs, lazyp }, a) = c_dbrhs (rhs, a)
      | c_db (MarkDb (arg, _), a) = c_db (arg, a)

    and c_dbrhs (Constrs def, a) = foldl (tyoption o' #2) a def
      | c_dbrhs (Repl cn, a) = s_addP (cn, a)

    and c_eb (EbGen { exn, etype }, a) = tyoption (etype, a)
      | c_eb (EbDef { exn, edef }, a) = s_addP (edef, a)
      | c_eb (MarkEb (arg, _), a) = c_eb (arg, a)

    and c_exp (VarExp p, a) = d_addP (p, a)
      | c_exp (FnExp arg, a) = foldr c_rule a arg
      | c_exp (FlatAppExp l, a) = foldr (c_exp o' #item) a l
      | c_exp (AppExp { function, argument }, a) =
	c_exp (function, c_exp (argument, a))
      | c_exp (CaseExp { expr, rules }, a) = c_exp (expr, foldr c_rule a rules)
      | c_exp (LetExp { dec, expr }, a) =
	localDec (do_dec (dec, []), c_exp (expr, []), a)
      | c_exp ((SeqExp l | ListExp l | TupleExp l | VectorExp l), a) =
	foldr c_exp a l
      | c_exp (RecordExp l, a) = foldr (c_exp o' #2) a l
      | c_exp (SelectorExp _, a) = a
      | c_exp (ConstraintExp { expr, constraint }, a) =
	c_exp (expr, d_addS (c_ty (constraint, SS.empty), a))
      | c_exp (HandleExp { expr, rules }, a) =
	c_exp (expr, foldr c_rule a rules)
      | c_exp (RaiseExp e, a) = c_exp (e, a)
      | c_exp (IfExp { test, thenCase, elseCase }, a) =
	c_exp (test, c_exp (thenCase, c_exp (elseCase, a)))
      | c_exp ((AndalsoExp (e1, e2) | OrelseExp (e1, e2)), a) =
	c_exp (e1, c_exp (e2, a))
      | c_exp (WhileExp { test, expr }, a) = c_exp (test, c_exp (expr, a))
      | c_exp (MarkExp (arg, _), a) = c_exp (arg, a)
      | c_exp ((IntExp _|WordExp _|RealExp _|StringExp _|CharExp _), a) = a
		
    and c_rule (Rule { pat, exp }, a) =
	d_addS (c_pat (pat, SS.empty), c_exp (exp, a))

    and c_pat (VarPat p, a) = s_addP (p, a)
      | c_pat (RecordPat { def, ... }, a) = foldl (c_pat o' #2) a def
      | c_pat ((ListPat l | TuplePat l | VectorPat l | OrPat l), a) =
	foldl c_pat a l
      | c_pat (FlatAppPat l, a) = foldl (c_pat o' #item) a l
      | c_pat (AppPat { constr, argument }, a) =
	c_pat (constr, c_pat (argument, a))
      | c_pat (ConstraintPat { pattern, constraint }, a) =
	c_pat (pattern, c_ty (constraint, a))
      | c_pat (LayeredPat { varPat, expPat }, a) =
	c_pat (varPat, c_pat (expPat, a))
      | c_pat (MarkPat (arg, _), a) = c_pat (arg, a)
      | c_pat ((WildPat|IntPat _|WordPat _|StringPat _|CharPat _), a) = a

    and c_ty (VarTy _, a) = a
      | c_ty (ConTy (cn, l), a) = s_addP (cn, foldl c_ty a l)
      | c_ty (RecordTy l, a) = foldl (c_ty o' #2) a l
      | c_ty (TupleTy l, a) = foldl c_ty a l
      | c_ty (MarkTy (arg, _), a) = c_ty (arg, a)

    and tyoption (NONE, a) = a
      | tyoption (SOME ty, a) = c_ty (ty, a)

    fun convert { tree, err } = let
	(* build a function that will complain (once you call it)
	 * about any existing restriction violations *)
	fun newReg reg = let
	    fun sameReg (LocalDec (_, body), k) = sameReg (body, k)
	      | sameReg (SeqDec l, k) = foldl sameReg k l
	      | sameReg (OpenDec _, k) =
		(fn () => (k (); err EM.COMPLAIN reg "toplevel open"))
	      | sameReg (MarkDec (arg, reg), k) = newReg reg (arg, k)
	      | sameReg ((StrDec _ | AbsDec _ | FctDec _ | SigDec _ |
			  FsigDec _), k) = k
	      | sameReg (_, k) =
		(fn () =>
		 (k (); err EM.WARN reg "definition not tracked by CM"))

	in
	    sameReg
	end
    in
	{ complain = newReg (0, 0) (tree, fn () => ()),
	  skeleton = SkelOpt.opt (c_dec tree) }
    end
end

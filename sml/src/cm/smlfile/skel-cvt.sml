(*
 * Convert ASTs to CM's trimmed version thereof.
 *
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
	-> Skeleton.decl
end

structure SkelCvt :> SKELCVT = struct

    open GenericVC.Ast Skeleton

    structure S = Symbol
    structure SP = GenericVC.SymPath
    structure SS = SymbolSet
    structure EM = GenericVC.ErrorMsg

    val symbolModPath = SP.SPATH

    type symbol = Symbol.symbol
    type path = symbol list

    fun allButLast lst =
	case lst of
	    [] => []
	  | [last] => []
	  | head :: (tail as (_ :: _)) => head :: (allButLast tail)

    fun modRef (path, accum) =
	case path of [] => accum
      | [only] => accum
      | head :: _ => SS.add (accum, head)

    fun declRef (path, accum) =
	case path of
	    [] => accum
	  | head :: _ =>
		(case accum of
		     [] => [DeclRef (SS.singleton head)]
		   | (DeclRef otherRefs) :: tail =>
			 (DeclRef (SS.add (otherRefs, head))) :: tail
		   | _ => (DeclRef (SS.singleton head)) :: accum)

    fun dropLast [x] = nil
      | dropLast [] = []
      | dropLast (a :: rest) = a :: (dropLast rest)

    fun modRefSet (modNames, accum) =
	if SS.isEmpty modNames then accum
	else
	    case accum of
		[] => [DeclRef modNames]
	      | (DeclRef otherRefs) :: tail =>
		    (DeclRef (SS.union (modNames, otherRefs))) :: tail
	      | _ => (DeclRef modNames) :: accum

    fun localDec ((bind, body), accum) =
	case (bind, body) of
	    ([], []) => accum
	  | ([], [DeclRef names]) => modRefSet (names, accum)
	  | ([DeclRef names], []) => modRefSet (names, accum)
	  | ([DeclRef names1], [DeclRef names2]) =>
		modRefSet (SS.union (names1, names2), accum)
	  | args => (LocalDecl (SeqDecl bind, SeqDecl body)) :: accum

    fun c_dec ast =
	case do_dec (ast, []) of
	    [] => DeclRef SS.empty
	  | [decl] => decl
	  | declList => SeqDecl declList

    and do_dec (ast, accum) =
	case ast of
	    ValDec (arg, _) => foldr c_vb accum arg
	  | ValrecDec (arg, _) => foldr c_rvb accum arg
	  | FunDec (arg, _) => foldr c_fb accum arg
	  | TypeDec arg => modRefSet (foldr c_tb SS.empty arg, accum)
	  | DatatypeDec { datatycs, withtycs } =>
		modRefSet (foldr c_db (foldr c_tb SS.empty withtycs) datatycs,
			   accum)
	  | AbstypeDec { abstycs, withtycs, body } =>
		(* body is syntactically restricted to ldecs,
		 * no module scoping here *)
		modRefSet (foldr c_db (foldr c_tb SS.empty withtycs) abstycs,
			   (c_dec body) :: accum)
	  | ExceptionDec arg =>
		modRefSet (foldr c_eb SS.empty arg, accum)
	  | StrDec arg => (StrDecl (foldr c_strb [] arg)) :: accum
	  | AbsDec arg => (StrDecl (foldr c_strb [] arg)) :: accum
	  | FctDec arg => (FctDecl (foldr c_fctb [] arg)) :: accum
	  | SigDec arg => (StrDecl (foldr c_sigb [] arg)) :: accum
	  | FsigDec arg => (FctDecl (foldr c_fsigb [] arg)) :: accum
	  | LocalDec (bindingDec, bodyDec) =>
		localDec ((do_dec (bindingDec, []),
			   do_dec (bodyDec, [])),
			  accum)
	  | SeqDec arg => foldr do_dec accum arg
 	  | OpenDec arg =>
		(OpenDecl (map (VarStrExp o symbolModPath) arg)) :: accum
	  | OvldDec arg => accum
	  | FixDec arg => accum
	  | MarkDec (arg, _) => do_dec (arg, accum)

    and c_strb (ast, accum) =
	case ast of
	    Strb { name, def, constraint } =>
		{
		 name = name,
		 def = c_strexp def,
		 constraint = sigexpConst constraint
		} :: accum
	  | MarkStrb (arg, _) => c_strb (arg, accum)

    and c_fctb (ast, accum) =
	case ast of
	    Fctb { name, def } => 
		{ name = name, def = c_fctexp def } :: accum
	  | MarkFctb (arg, _) => c_fctb (arg, accum)

    and c_sigb (ast, accum) =
	case ast of
	    Sigb { name, def } =>
		{
		 name = name,
		 def = c_sigexp def,
		 constraint = NONE
		} :: accum
	  | MarkSigb (arg, _) => c_sigb (arg, accum)

    and c_fsigb (ast, accum) =
	case ast of
	    Fsigb { name, def } =>
		{ name = name, def = c_fsigexp def } :: accum
	  | MarkFsigb (arg, _) => c_fsigb (arg, accum)

    and c_strexp ast =
	case ast of
	    VarStr path => VarStrExp (symbolModPath path)
	  | BaseStr dec => BaseStrExp (c_dec dec)
 	  | ConstrainedStr (strexp,NoSig) => c_strexp strexp
 	  | ConstrainedStr (strexp, (Transparent sigexp | Opaque sigexp)) =>
 		ConStrExp (c_strexp strexp, c_sigexp sigexp)
	  | (AppStr (path, argList) |
	     AppStrI (path, argList)) =>
		AppStrExp (symbolModPath path,
			   map (fn (se, _) => c_strexp se) argList)
	  | LetStr (bindings, body) =>
		LetStrExp (c_dec bindings, c_strexp body)
	  | MarkStr (strexp, _) => c_strexp strexp

    and c_fctexp ast =
	case ast of
	    VarFct (path, constraint) =>
		VarFctExp (symbolModPath path, fsigexpConst constraint)
	  | BaseFct { params, body, constraint } =>
		BaseFctExp {
			    params = map functorParams params,
			    body = c_strexp body,
			    constraint = sigexpConst constraint
			   }
	  | AppFct (path, argList, constraint) =>
		AppFctExp (symbolModPath path,
			   map (fn (se, _) => c_strexp se) argList,
			   fsigexpConst constraint)
	  | LetFct (bindings, body) =>
		LetFctExp (c_dec bindings, c_fctexp body)
	  | MarkFct (arg, _) => c_fctexp arg

    and functorParams (symOpt, constraint) = let
	val c = c_sigexp constraint
    in
	case symOpt of
	    NONE => (NONE,c)
	  | SOME sym => (SOME sym, c)
    end

    and sigexpConst sec =
	case sec of
	    NoSig => NONE
	  | Transparent sigexp => SOME (c_sigexp sigexp)
	  | Opaque sigexp => SOME (c_sigexp sigexp)

    and c_sigexp ast =
	case ast of
	    VarSig symbol => VarStrExp (symbolModPath [symbol])
	  | AugSig (se, whspecs) => let
		fun f (WhType (_, _, ty), x) = c_ty (ty, x)
		  | f (WhStruct (_, head :: _), x) =
		    SS.add (x, head)
		  | f _ = raise Fail "decl/convert/c_sigexp" 
	    in
		AugStrExp (c_sigexp se, foldr f SS.empty whspecs)
	    end
	  | BaseSig specList =>
		BaseStrExp (SeqDecl (foldr c_spec [] specList))
	  | MarkSig (arg,_) => c_sigexp arg

    and fsigexpConst arg =
	case arg of
	    NoSig => NONE
	  | Transparent fsigexp => SOME (c_fsigexp fsigexp)
	  | Opaque fsigexp => SOME (c_fsigexp fsigexp)

    and c_fsigexp ast =
	case ast of
	    VarFsig symbol => VarFctExp (symbolModPath [symbol], NONE)
	  | BaseFsig { param, result } =>
		BaseFctExp {
			    params = map functorParams param,
			    body = c_sigexp result,
			    constraint = NONE
			   }
	  | MarkFsig (arg, _) => c_fsigexp arg

    and c_spec (ast, accum) =
	case ast of
	    StrSpec arg => let
 		fun f (symbol, sigexp, NONE) =
		    {
		     name = symbol,
		     def = c_sigexp sigexp,
		     constraint = NONE
		    }
		  | f (symbol, sigexp, SOME path) =
		    {
		     name = symbol,
		     def = VarStrExp (symbolModPath path),
		     constraint = SOME(c_sigexp sigexp)
		    }
	    in
		(StrDecl (map f arg)) :: accum
	    end
	  | TycSpec (arg, _) => let
		fun filter ((_, _, SOME x) :: rest) = x :: filter rest
		  | filter (_ :: rest) = filter rest
		  | filter nil = nil
		val mod'ref'set = foldr c_ty SS.empty (filter arg)
	    in
		modRefSet (mod'ref'set, accum)
	    end
	  | FctSpec arg => let
		fun f (symbol, fsigexp) =
		    { name = symbol, def = c_fsigexp fsigexp }
	    in
		(FctDecl (map f arg)) :: accum
	    end
	  | ValSpec arg => let
		val mod'ref'set = foldr c_ty SS.empty (map #2 arg)
	    in
		modRefSet (mod'ref'set, accum)
	    end
	  | DataSpec { datatycs, withtycs } =>
		modRefSet (foldr c_db (foldr c_tb SS.empty withtycs) datatycs,
			   accum)
	  | ExceSpec arg => let
		val mod'ref'set = foldr tyoption SS.empty (map #2 arg)
	    in
		modRefSet (mod'ref'set, accum)
	    end
	  | ShareStrSpec arg => foldr declRef accum arg
	  | ShareTycSpec arg => foldr declRef accum (map dropLast arg)
 	  | IncludeSpec sigexp => (OpenDecl [c_sigexp sigexp]) :: accum
	  | MarkSpec (arg, _) => c_spec (arg, accum)

    and c_vb (ast, accum) =
	case ast of
	    Vb { pat, exp, lazyp } =>
		modRefSet (c_pat (pat, SS.empty), c_exp (exp, accum))
	  | MarkVb (arg, _) => c_vb (arg, accum)

    and c_rvb (ast, accum) =
	case ast of
	    Rvb { var, exp, resultty,... } =>
		modRefSet (tyoption (resultty, SS.empty), c_exp (exp, accum))
	  | MarkRvb (arg, _) => c_rvb (arg, accum)

    and c_fb (ast, accum) =
	case ast of
	    Fb (clauses, _) => foldr c_clause accum clauses
	  | MarkFb (arg,_) => c_fb (arg, accum)

    and c_clause (Clause { pats, resultty, exp }, accum) =
	modRefSet
	  (foldr c_pat (tyoption (resultty, SS.empty)) (map #item pats),
	   c_exp (exp, accum))

    and c_tb (ast, accum) =
	case ast of
	    Tb { tyc, def, tyvars } => c_ty (def, accum)
	  | MarkTb (arg, _) => c_tb (arg, accum)

    and c_db (ast, accum) =
	case ast of
	    Db { tyc, tyvars, rhs, lazyp } => c_dbrhs (rhs, accum)
	  | MarkDb (arg, _) => c_db (arg, accum)

    and c_dbrhs (ast,accum) =
	case ast of
	    Constrs def => foldr tyoption accum (map #2 def)
          | Repl consName => modRef (consName, accum)

    and c_eb (ast, accum) =
	case ast of
	    EbGen { exn, etype } => tyoption (etype, accum)
	  | EbDef { exn, edef } => modRef (edef, accum)
	  | MarkEb (arg, _) => c_eb (arg, accum)

    and c_exp (ast, accum) =
	case ast of
	    VarExp path =>
		(case path of
		     [] => accum
		   | [only] => accum
		   | head :: _ =>
			 (case accum of
			      [] => [DeclRef (SS.singleton head)]
			    | (DeclRef otherRefs) :: tail =>
				  (DeclRef (SS.add (otherRefs, head))) :: tail
			    | _ => (DeclRef (SS.singleton head)) :: accum))
	  | FnExp arg => foldr c_rule accum arg
	  | FlatAppExp items => foldr c_exp accum (map #item items)
	  | AppExp { function, argument } =>
		c_exp (function, c_exp (argument, accum))
	  | CaseExp {expr, rules } =>
		c_exp (expr, foldr c_rule accum rules)
	  | LetExp { dec, expr } =>
		(* syntactically only ldecs; no module scoping here *)
		localDec ((do_dec (dec, []), c_exp (expr, [])), accum)
	  | SeqExp arg => foldr c_exp accum arg
	  | RecordExp arg  => foldr c_exp accum (map #2 arg)
	  | ListExp arg => foldr c_exp accum arg
	  | TupleExp arg => foldr c_exp accum arg
	  | SelectorExp symbol => accum
	  | ConstraintExp { expr, constraint } =>
		c_exp (expr, modRefSet (c_ty (constraint, SS.empty), accum))
	  | HandleExp { expr, rules } =>
		c_exp (expr, foldr c_rule accum rules)
	  | RaiseExp expr => c_exp (expr, accum)
	  | IfExp { test, thenCase, elseCase } =>
		c_exp (test, c_exp (thenCase, c_exp (elseCase, accum)))
	  | AndalsoExp (expr1, expr2) => c_exp (expr1, c_exp (expr2, accum))
	  | OrelseExp (expr1, expr2) => c_exp (expr1, c_exp (expr2, accum))
	  | WhileExp { test, expr } => c_exp (test, c_exp (expr, accum))
	  | MarkExp (arg, _) => c_exp (arg, accum)
	  | VectorExp arg => foldr c_exp accum arg
	  | _ => accum
		
    and c_rule (Rule { pat, exp }, accum) =
	modRefSet (c_pat (pat, SS.empty), c_exp (exp, accum))

    and c_pat (ast, accum) =
	case ast of
	    VarPat path => modRef (path, accum)
	  | RecordPat { def, ... } => foldr c_pat accum (map #2 def)
	  | ListPat arg => foldr c_pat accum arg
	  | TuplePat arg => foldr c_pat accum arg
	  | FlatAppPat items => foldr c_pat accum (map #item items)
	  | AppPat { constr, argument } =>
		c_pat (constr, c_pat (argument, accum))
	  | ConstraintPat { pattern, constraint } =>
		c_pat (pattern, c_ty (constraint, accum))
	  | LayeredPat { varPat, expPat } =>
		c_pat (varPat, c_pat (expPat, accum))
	  | VectorPat arg => foldr c_pat accum arg
	  | OrPat arg => foldr c_pat accum arg
	  | MarkPat (arg, _) => c_pat (arg, accum)
	  | _ => accum

    and c_ty (ast, accum) =
	case ast of
	    VarTy arg => accum
	  | ConTy (consName, args) =>
		modRef (consName, foldr c_ty accum args)
	  | RecordTy arg => foldr c_ty accum (map #2 arg)
	  | TupleTy arg => foldr c_ty accum arg
	  | MarkTy (arg, _) => c_ty (arg, accum)

    and tyoption (arg, accum) =
	case arg of
	    NONE => accum
	  | SOME ty => c_ty (ty, accum)

    fun check_toplevel (ast, err) = let
	fun check_topl (StrDec _, _) = ()
	  | check_topl (AbsDec _, _) = ()
	  | check_topl (FctDec _, _) = ()
	  | check_topl (SigDec _, _) = ()
	  | check_topl (FsigDec _, _) = ()
	  | check_topl (LocalDec (_, body), reg) = check_topl (body, reg)
	  | check_topl (SeqDec arg, reg) =
	    app (fn ast => check_topl (ast, reg)) arg
	  | check_topl (OpenDec _, reg) = err EM.COMPLAIN reg "toplevel open"
	  | check_topl (MarkDec (arg, reg), _) = check_topl (arg, reg)
	  | check_topl (_, reg) =
	    err EM.WARN reg "definition not tracked by CM"
    in
	check_topl (ast, (0, 0))
    end

    fun convert { tree, err } = (check_toplevel (tree, err); c_dec tree)

end

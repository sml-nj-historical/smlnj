(*
 * semantic actions to go with the grammar for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CM_SEMANT = sig

    type context = SrcPath.dir
    type region = GenericVC.SourceMap.region
    type ml_symbol
    type cm_symbol
    type cm_class
    type cm_version = Version.t

    type group = GroupGraph.group

    type privilegespec
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    type toolopt

    type complainer = string -> unit

    (* getting elements of primitive types (pathnames and symbols) *)
    val file_native : string * context * complainer -> SrcPath.prefile
    val file_standard :
	GeneralParams.info -> string * context * complainer -> SrcPath.prefile
    val cm_symbol : string -> cm_symbol
    val cm_version : string * complainer -> cm_version
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol
    val class : cm_symbol -> cm_class

    (* getting the full analysis for a group/library *)
    val group : { path: SrcPath.file,
		  privileges: privilegespec,
		  exports: exports option,
		  members: members,
		  gp: GeneralParams.info,
		  curlib: SrcPath.file option,
		  owner: SrcPath.file option,
		  error: complainer,
		  initgroup: group } -> group
    val library : { path: SrcPath.file,
		    privileges: privilegespec,
		    exports: exports,
		    version : cm_version option,
		    members: members,
		    gp: GeneralParams.info,
		    initgroup: group } -> group

    (* assembling privilege lists *)
    val initialPrivilegeSpec : privilegespec
    val require : privilegespec * cm_symbol * complainer -> privilegespec
    val wrap : privilegespec * cm_symbol * complainer -> privilegespec

    (* constructing member collections *)
    val emptyMembers : members
    val member :
	{ gp: GeneralParams.info,
	  rparse: SrcPath.file option ->
		  SrcPath.file * Version.t option * SrcPath.rebindings ->
		  group,
	  load_plugin: SrcPath.dir -> string -> bool }
	-> { name: string,
	     mkpath: string -> SrcPath.prefile,
	     group: SrcPath.file * region,
	     class: cm_class option,
	     tooloptions: toolopt list option,
	     context: SrcPath.dir }
	-> members
    val members : members * members -> members
    val guarded_members :
	exp * (members * members) * (string -> unit) -> members
    val error_member : (unit -> unit) -> members

    (* constructing export lists *)
    val emptyExports : exports
    val export : ml_symbol * complainer -> exports
    val exports : exports * exports -> exports
    val guarded_exports :
	exp * (exports * exports) * (string -> unit) -> exports
    val error_export : (unit -> unit) -> exports

    (* groups of operator symbols (to make grammar smaller) *)
    type addsym
    val PLUS : addsym
    val MINUS : addsym
    
    type mulsym
    val TIMES : mulsym
    val DIV : mulsym
    val MOD : mulsym

    type eqsym
    val EQ : eqsym
    val NE : eqsym

    type ineqsym
    val GT : ineqsym
    val GE : ineqsym
    val LT : ineqsym
    val LE : ineqsym

    (* arithmetic (number-valued) expression *)
    val number : int -> aexp
    val variable : GeneralParams.info -> cm_symbol -> aexp
    val add : aexp * addsym * aexp -> aexp
    val mul : aexp * mulsym * aexp -> aexp
    val sign : addsym * aexp -> aexp
    val negate : aexp -> aexp

    (* (bool-valued) expressions *)
    val ml_defined : ml_symbol -> exp
    val cm_defined : GeneralParams.info -> cm_symbol -> exp
    val conj : exp * exp -> exp
    val disj : exp * exp -> exp
    val beq : exp * eqsym * exp -> exp
    val not : exp -> exp
    val ineq : aexp * ineqsym * aexp -> exp
    val eq : aexp * eqsym * aexp -> exp

    (* tool options *)
    val string : { name: string, mkpath: string -> SrcPath.prefile } -> toolopt
    val subopts : { name: string, opts: toolopt list } -> toolopt
end

structure CMSemant :> CM_SEMANT = struct

    structure SymPath = GenericVC.SymPath
    structure EM = GenericVC.ErrorMsg
    structure GG = GroupGraph

    type context = SrcPath.dir
    type region = GenericVC.SourceMap.region
    type ml_symbol = Symbol.symbol
    type cm_symbol = string
    type cm_class = string
    type cm_version = Version.t

    type group = GG.group
    type privilegespec = { required: GG.privileges, wrapped: GG.privileges }

    type environment = MemberCollection.collection

    type aexp = environment -> int
    type exp = environment -> bool
    type members =
	 environment * SrcPath.file option -> MemberCollection.collection
    type exports = environment -> SymbolSet.set

    type toolopt = PrivateTools.toolopt

    type complainer = string -> unit

    fun saveEval (exp, env, error) =
	exp env
	handle exn =>
	    (error ("expression raises exception: " ^ General.exnMessage exn);
	     false)

    fun file_native (s, d, err) =
	SrcPath.native { err = err } { context = d, spec = s }
    fun file_standard (gp: GeneralParams.info) (s, d, err) =
	SrcPath.standard { env = #penv (#param gp), err = err }
			 { context = d, spec = s }
    fun cm_symbol s = s
    fun cm_version (s, error) =
	case Version.fromString s of
	    SOME v => v
	  | NONE => (error "ill-formed version specification"; Version.zero)
    val ml_structure = Symbol.strSymbol
    val ml_signature = Symbol.sigSymbol
    val ml_functor = Symbol.fctSymbol
    val ml_funsig = Symbol.fsigSymbol

    fun class s = String.map Char.toLower s

    fun applyTo mc e = e mc

    fun sgl2sll subgroups = let
	fun sameSL (p, _, _) (p', _, _) = SrcPath.compare (p, p') = EQUAL
	fun add (x, l) =
	    if List.exists (sameSL x) l then l else x :: l
	fun oneSG (x as (_, gth, _), l) =
	    case gth () of
		GG.GROUP { kind, sublibs, ... } =>
		(case kind of
		     GG.NOLIB _ => foldl add l sublibs
		   | _ => add (x, l))
	      | _ => l
    in
	foldl oneSG [] subgroups
    end

    fun group arg = let
	val { path = g, privileges = p, exports = e, members = m,
	      gp, curlib, owner, error, initgroup } = arg
	val mc = applyTo (MemberCollection.implicit gp initgroup, curlib) m
	val filter = Option.map (applyTo mc) e
	val pfsbn = let
	    val { exports, ... } =
		case initgroup of
		    GG.GROUP x => x
		  | GG.ERRORGROUP =>
		    EM.impossible "semant.sml: group: bad init group"
	in
	    #1 (valOf (SymbolMap.find (exports, PervAccess.pervStrSym)))
	end
	val (exports, rp) = MemberCollection.build (mc, filter, gp, pfsbn ())
	fun thunkify (p, g, rb) = (p, fn () => g, rb)
	val subgroups = map thunkify (MemberCollection.subgroups mc)
	val { required = rp', wrapped = wr } = p
	val rp'' = StringSet.union (rp', StringSet.union (rp, wr))
    in
	if StringSet.isEmpty wr then ()
	else EM.impossible "group with wrapped privileges";
	GG.GROUP { exports = exports,
		   kind = GG.NOLIB { subgroups = subgroups, owner = owner },
		   required = rp'',
		   grouppath = g,
		   sources = MemberCollection.sources mc,
		   sublibs = sgl2sll subgroups }
    end

    fun library arg = let
	val { path = g, privileges = p, exports = e, members = m,
	      version, gp, initgroup } = arg
	val mc = applyTo (MemberCollection.implicit gp initgroup, SOME g) m
	val filter = SOME (applyTo mc e)
	val pfsbn = let
	    val { exports, ... } =
		case initgroup of
		    GG.GROUP x => x
		  | GG.ERRORGROUP =>
		    EM.impossible "semant.sml: lib: bad init group"
	in
	    #1 (valOf (SymbolMap.find (exports, PervAccess.pervStrSym)))
	end
	val (exports, rp) = MemberCollection.build (mc, filter, gp, pfsbn ())
	fun thunkify (p, g, rb) = (p, fn () => g, rb)
	val subgroups = map thunkify (MemberCollection.subgroups mc)
	val { required = rp', wrapped = wr } = p
	val rp'' = StringSet.union (rp', StringSet.union (rp, wr))
    in
	GG.GROUP { exports = exports,
		   kind = GG.LIB { version = version,
				   kind = GG.DEVELOPED { subgroups = subgroups,
							 wrapped = wr } },
		   required = rp'',
		   grouppath = g,
		   sources = MemberCollection.sources mc,
		   sublibs = sgl2sll subgroups }
    end

    local
	val isMember = StringSet.member
	fun sanity ({ required, wrapped }, s, error) =
	    if isMember (required, s) orelse isMember (wrapped, s) then
		error ("duplicate privilege name: " ^ s)
	    else ()
    in
	val initialPrivilegeSpec = { required = StringSet.empty,
				     wrapped = StringSet.empty }
	fun require (a as ({ required, wrapped }, s, _)) =
	    (sanity a;
	     { required = StringSet.add (required, s), wrapped = wrapped })
	fun wrap (a as ({ required, wrapped }, s, _)) =
	    (sanity a;
	     { required = required, wrapped = StringSet.add (wrapped, s) })
    end

    fun emptyMembers (env, _) = env
    fun member { gp, rparse, load_plugin } arg (env, curlib) = let
	val coll = MemberCollection.expandOne
		       { gp = gp, rparse = rparse curlib,
			 load_plugin = load_plugin }
		       arg
	val group = #group arg
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	fun checkowner (_, GG.GROUP { kind = GG.NOLIB { owner, ... }, ...},
			_) =
	    let fun libname NONE = "<toplevel>"
		  | libname (SOME p) = SrcPath.descr p
		fun eq (NONE, NONE) = true
		  | eq (SOME p, SOME p') = SrcPath.compare (p, p') = EQUAL
		  | eq _ = false
	    in
		if eq (curlib, owner) then ()
		else e0 (concat ["owner of subgroup (", libname owner,
				 ") does not match current library (",
				 libname curlib, ")"])
	    end
	  | checkowner _ = ()
    in
	app checkowner (MemberCollection.subgroups coll);
	MemberCollection.sequential (env, coll, e0)
    end
    fun members (m1, m2) (env, curlib) = m2 (m1 (env, curlib), curlib)
    fun guarded_members (c, (m1, m2), error) (env, curlib) =
	if saveEval (c, env, error) then m1 (env, curlib) else m2 (env, curlib)
    fun error_member thunk (env, _) = (thunk (); env)

    fun emptyExports env = SymbolSet.empty
    fun export (s, error) env =
	if MemberCollection.ml_look env s then SymbolSet.singleton s
	else (error (concat ["exported ",
			     Symbol.nameSpaceToString (Symbol.nameSpace s),
			     " not defined: ", Symbol.name s]);
	      SymbolSet.empty)
    fun exports (e1, e2) env = SymbolSet.union (e1 env, e2 env)
    fun guarded_exports (c, (e1, e2), error) env =
	if saveEval (c, env, error) then e1 env else e2 env
    fun error_export thunk env = (thunk (); SymbolSet.empty)

    datatype addsym = PLUS | MINUS
    datatype mulsym = TIMES | DIV | MOD
    datatype eqsym = EQ | NE
    datatype ineqsym = GT | GE | LT | LE

    fun number i _ = i
    fun variable gp v e = MemberCollection.num_look gp e v
    fun add (e1, PLUS, e2) e = e1 e + e2 e
      | add (e1, MINUS, e2) e = e1 e - e2 e
    fun mul (e1, TIMES, e2) e = e1 e * e2 e
      | mul (e1, DIV, e2) e = e1 e div e2 e
      | mul (e1, MOD, e2) e = e1 e mod e2 e
    fun sign (PLUS, ex) e = ex e
      | sign (MINUS, ex) e = ~(ex e)
    fun negate ex e = ~(ex e)

    fun ml_defined s e = MemberCollection.ml_look e s
    fun cm_defined gp s e = MemberCollection.cm_look gp e s
    fun conj (e1, e2) e = e1 e andalso e2 e
    fun disj (e1, e2) e = e1 e orelse e2 e
    fun beq (e1: exp, EQ, e2) e = e1 e = e2 e
      | beq (e1, NE, e2) e = e1 e <> e2 e
    fun not ex e = Bool.not (ex e)
    fun ineq (e1, LT, e2) e = e1 e < e2 e
      | ineq (e1, LE, e2) e = e1 e <= e2 e
      | ineq (e1, GT, e2) e = e1 e > e2 e
      | ineq (e1, GE, e2) e = e1 e >= e2 e
    fun eq (e1: aexp, EQ, e2) e = e1 e = e2 e
      | eq (e1, NE, e2) e = e1 e <> e2 e

    val string = PrivateTools.STRING
    val subopts = PrivateTools.SUBOPTS
end

(*
 * semantic actions to go with the grammar for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CM_SEMANT = sig

    type context = SrcPath.context
    type pathname = SrcPath.t
    type region = GenericVC.SourceMap.region
    type ml_symbol
    type cm_symbol
    type cm_class

    type group = GroupGraph.group

    type privilegespec
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    type complainer = string -> unit

    (* getting elements of primitive types (pathnames and symbols) *)
    val file_native : string * context -> pathname
    val file_standard : GeneralParams.info -> string * context -> pathname
    val cm_symbol : string -> cm_symbol
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol
    val class : cm_symbol -> cm_class

    (* getting the full analysis for a group/library *)
    val emptyGroup : pathname -> group
    val group :
	pathname * privilegespec * exports option * members *
	GeneralParams.info * pathname option * pathname option * complainer *
	GroupGraph.group		(* init group *)
	-> group
    val library :
	pathname * privilegespec * exports * members *
	GeneralParams.info *
	GroupGraph.group		(* init group *)
	-> group

    (* assembling privilege lists *)
    val initialPrivilegeSpec : privilegespec
    val require : privilegespec * cm_symbol * complainer -> privilegespec
    val wrap : privilegespec * cm_symbol * complainer -> privilegespec

    (* constructing member collections *)
    val emptyMembers : members
    val member :
	GeneralParams.info * (pathname option -> pathname -> group) *
	                     (SrcPath.context -> string -> bool)
	-> { name: string, mkpath: string -> pathname,
	     group: pathname * region, class: cm_class option,
	     context: SrcPath.context }
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

    (* arithmetic (number-valued) expression *)
    val number : int -> aexp
    val variable : GeneralParams.info -> cm_symbol -> aexp
    val plus : aexp * aexp -> aexp
    val minus : aexp * aexp -> aexp
    val times : aexp * aexp -> aexp
    val divide : aexp * aexp -> aexp
    val modulus : aexp * aexp -> aexp
    val negate : aexp -> aexp

    (* (bool-valued) expressions *)
    val ml_defined : ml_symbol -> exp
    val cm_defined : GeneralParams.info -> cm_symbol -> exp
    val conj : exp * exp -> exp
    val disj : exp * exp -> exp
    val beq : exp * exp -> exp
    val bne : exp * exp -> exp
    val not : exp -> exp
    val lt : aexp * aexp -> exp
    val le : aexp * aexp -> exp
    val gt : aexp * aexp -> exp
    val ge : aexp * aexp -> exp
    val eq : aexp * aexp -> exp
    val ne : aexp * aexp -> exp
end

structure CMSemant :> CM_SEMANT = struct

    structure SymPath = GenericVC.SymPath
    structure EM = GenericVC.ErrorMsg
    structure GG = GroupGraph

    type pathname = SrcPath.t
    type context = SrcPath.context
    type region = GenericVC.SourceMap.region
    type ml_symbol = Symbol.symbol
    type cm_symbol = string
    type cm_class = string

    type group = GG.group
    type privilegespec = { required: GG.privileges, wrapped: GG.privileges }

    type environment = MemberCollection.collection

    type aexp = environment -> int
    type exp = environment -> bool
    type members = environment * pathname option -> MemberCollection.collection
    type exports = environment -> SymbolSet.set

    type complainer = string -> unit

    fun saveEval (exp, env, error) =
	exp env
	handle exn =>
	    (error ("expression raises exception: " ^ General.exnMessage exn);
	     false)

    fun file_native (s, d) = SrcPath.native { context = d, spec = s }
    fun file_standard (gp: GeneralParams.info) (s, d) =
	SrcPath.standard (#pcmode (#param gp)) { context = d, spec = s }
    fun cm_symbol s = s
    val ml_structure = Symbol.strSymbol
    val ml_signature = Symbol.sigSymbol
    val ml_functor = Symbol.fctSymbol
    val ml_funsig = Symbol.fsigSymbol

    fun class s = String.map Char.toLower s

    fun applyTo mc e = e mc

    fun emptyGroup path =
	GG.GROUP { exports = SymbolMap.empty,
		   kind = GG.NOLIB { subgroups = [], owner = NONE },
		   required = StringSet.empty,
		   grouppath = path,
		   sublibs = [] }

    fun sgl2sll subgroups = let
	fun sameSL (p, g) (p', g') = SrcPath.compare (p, p') = EQUAL
	fun add (x, l) =
	    if List.exists (sameSL x) l then l else x :: l
	fun oneSG (x as (_, GG.GROUP { kind, sublibs, ... }), l) =
	    case kind of
		GG.NOLIB _ => foldl add l sublibs
	      | _ => add (x, l)
    in
	foldl oneSG [] subgroups
    end

    fun grouplib (isgroup, g, p, e, m, gp, curlib, init_group) = let
	val mc = applyTo (MemberCollection.implicit init_group, curlib) m
	val filter = Option.map (applyTo mc) e
	val pfsbn = let
	    val GroupGraph.GROUP { exports, ... } = init_group
	in
	    #1 (valOf (SymbolMap.find (exports, PervCoreAccess.pervStrSym)))
	end
	val (exports, rp) = MemberCollection.build (mc, filter, gp, pfsbn)
	val subgroups = MemberCollection.subgroups mc
	val { required = rp', wrapped = wr } = p
	val rp'' = StringSet.union (rp', StringSet.union (rp, wr))
    in
	GG.GROUP { exports = exports,
		   kind = case isgroup of
			      NONE => GG.LIB { wrapped = wr,
					       subgroups = subgroups }
			    | SOME owner => 
			      (if StringSet.isEmpty wr then ()
			       else EM.impossible
					"group with wrapped privilege";
					GG.NOLIB { subgroups = subgroups,
						   owner = owner }),
		   required = rp'',
		   grouppath = g,
		   sublibs = sgl2sll subgroups }
    end

    fun group (g, p, e, m, gp, curlib, owner, error, init_group) =
	grouplib (SOME owner, g, p, e, m, gp, curlib, init_group)
    fun library (g, p, e, m, gp, init_group) =
	grouplib (NONE, g, p, SOME e, m, gp, SOME g, init_group)

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
    fun member (gp, rparse, ldpi) arg (env, curlib) = let
	val coll = MemberCollection.expandOne (gp, rparse curlib, ldpi) arg
	val group = #group arg
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
	fun checkowner (_, GG.GROUP { kind = GG.NOLIB { owner, ... }, ...}) =
	    let	fun libname NONE = "<toplevel>"
		  | libname (SOME p) = SrcPath.descr p
		fun eq (NONE, NONE) = true
		  | eq (SOME p, SOME p') = SrcPath.compare (p, p') = EQUAL
		  | eq _ = false
	    in
		if eq (curlib, owner) then ()
		else e0 (concat ["owner of subgroup (",
				 libname owner,
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

    fun number i _ = i
    fun variable gp v e = MemberCollection.num_look gp e v
    fun plus (e1, e2) e = e1 e + e2 e
    fun minus (e1, e2) e = e1 e - e2 e
    fun times (e1, e2) e = e1 e * e2 e
    fun divide (e1, e2) e = e1 e div e2 e
    fun modulus (e1, e2) e = e1 e mod e2 e
    fun negate ex e = ~(ex e)

    fun ml_defined s e = MemberCollection.ml_look e s
    fun cm_defined gp s e = MemberCollection.cm_look gp e s
    fun conj (e1, e2) e = e1 e andalso e2 e
    fun disj (e1, e2) e = e1 e orelse e2 e
    fun beq (e1: exp, e2) e = e1 e = e2 e
    fun bne (e1: exp, e2) e = e1 e <> e2 e
    fun not ex e = Bool.not (ex e)
    fun lt (e1, e2) e = e1 e < e2 e
    fun le (e1, e2) e = e1 e <= e2 e
    fun gt (e1, e2) e = e1 e > e2 e
    fun ge (e1, e2) e = e1 e >= e2 e
    fun eq (e1: aexp, e2) e = e1 e = e2 e
    fun ne (e1: aexp, e2) e = e1 e <> e2 e
end

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

    (* getting the full analysis for a group/library *)
    val emptyGroup : pathname -> group
    val group :
	pathname * privilegespec * exports option * members *
	GeneralParams.info
	-> group
    val library :
	pathname * privilegespec * exports * members *
	GeneralParams.info
	-> group

    (* assembling privilege lists *)
    val initialPrivilegeSpec : privilegespec
    val require : privilegespec * cm_symbol * complainer -> privilegespec
    val wrap : privilegespec * cm_symbol * complainer -> privilegespec

    (* constructing member collections *)
    val emptyMembers : members
    val member :
	GeneralParams.info * (pathname -> group)
	-> { sourcepath: pathname, group: pathname * region,
	     class: cm_symbol option }
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

    type group = GG.group
    type privilegespec = { required: GG.privileges, wrapped: GG.privileges }

    type environment = MemberCollection.collection

    type aexp = environment -> int
    type exp = environment -> bool
    type members = environment -> MemberCollection.collection
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

    fun applyTo mc e = e mc

    fun emptyGroup path =
	GG.GROUP { exports = SymbolMap.empty,
		   kind = GG.NOLIB,
		   required = StringSet.empty,
		   grouppath = path,
		   sublibs = [] }

    fun sgl2sll subgroups = let
	fun sameSL (_, GG.GROUP g) (_, GG.GROUP g') =
	    SrcPath.compare (#grouppath g, #grouppath g') = EQUAL
	fun add (x, l) =
	    if List.exists (sameSL x) l then l else x :: l
	fun oneSG (x as (_, GG.GROUP { kind, sublibs, ... }), l) =
	    case kind of
		GG.NOLIB => foldl add l sublibs
	      | _ => add (x, l)
    in
	foldl oneSG [] subgroups
    end

    fun grouplib (islib, g, p, e, m, gp) = let
	val mc = applyTo MemberCollection.empty m
	val filter = Option.map (applyTo mc) e
	val (exports, rp) = MemberCollection.build (mc, filter, gp)
	val subgroups = MemberCollection.subgroups mc
	val { required = rp', wrapped = wr } = p
	val rp'' = StringSet.union (rp', StringSet.union (rp, wr))
    in
	GG.GROUP { exports = exports,
		   kind = if islib then GG.LIB wr
			  else (if StringSet.isEmpty wr then ()
				else EM.impossible
				    "group with wrapped privilege";
				GG.NOLIB),
		   required = rp'',
		   grouppath = g,
		   sublibs = sgl2sll subgroups }
    end

    fun group (g, p, e, m, gp) =
	grouplib (false, g, p, e, m, gp)
    fun library (g, p, e, m, gp) =
	grouplib (true, g, p, SOME e, m, gp)

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

    fun emptyMembers env = env
    fun member (gp, rparse) arg env = let
	val coll = MemberCollection.expandOne (gp, rparse) arg
	val group = #group arg
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
    in
	MemberCollection.sequential (env, coll, e0)
    end
    fun members (m1, m2) env = m2 (m1 env)
    fun guarded_members (c, (m1, m2), error) env =
	if saveEval (c, env, error) then m1 env else m2 env
    fun error_member thunk env = (thunk (); env)

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

(*
 * semantic actions to go with the grammar for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CM_SEMANT = sig

    exception ExplicitError of string
    exception ExpressionError of exn

    type pathname
    type ml_symbol
    type cm_symbol

    type group

    type perms
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    type complainer = string -> unit

    (* getting elements of primitive types (pathnames and symbols) *)
    val file_native : string * pathname -> pathname
    val file_standard : string * pathname -> pathname
    val cm_symbol : string -> cm_symbol
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol

    (* getting the full analysis for a group/library (or an alias thereof) *)
    val alias : pathname -> group
    val group : perms * exports * members -> group
    val library : perms * exports * members -> group

    (* assembling permission lists *)
    val initialPerms : perms
    val require : perms * cm_symbol * complainer -> perms
    val grant : perms * cm_symbol * complainer -> perms

    (* constructing member collections *)
    val emptyMembers : members
    val member : pathname * cm_symbol option -> members
    val members : members * members -> members
    val guarded_members : exp * (members * members) -> members
    val error_member : string -> members

    (* constructing export lists *)
    val emptyExports : exports
    val export : ml_symbol -> exports
    val exports : exports * exports -> exports
    val guarded_exports : exp * (exports * exports) -> exports
    val error_export : string -> exports

    (* arithmetic (number-valued) expression *)
    val number : int -> aexp
    val variable : cm_symbol -> aexp
    val plus : aexp * aexp -> aexp
    val minus : aexp * aexp -> aexp
    val times : aexp * aexp -> aexp
    val divide : aexp * aexp -> aexp
    val modulus : aexp * aexp -> aexp
    val negate : aexp -> aexp

    (* (bool-valued) expressions *)
    val ml_defined : ml_symbol -> exp
    val cm_defined : cm_symbol -> exp
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

    exception ExplicitError of string
    exception ExpressionError of exn

    structure Symbol = GenericVC.Symbol
    structure SymPath = GenericVC.SymPath

    type pathname = AbsPath.t
    type ml_symbol = Symbol.symbol
    type cm_symbol = string

    type group = unit

    type environment = MemberCollection.environment

    type perms = { required : StringSet.set, granted : StringSet.set }

    type aexp = environment -> int
    type exp = environment -> bool
    type members = environment -> MemberCollection.collection
    type exports = environment -> SymbolSet.set

    type complainer = string -> unit

    fun saveEval (exp, env) =
	exp env
	handle exn =>
	    raise ExpressionError exn

    fun file_native (s, d) = AbsPath.native { context = d, spec = s }
    fun file_standard (s, d) = AbsPath.standard { context = d, spec = s }
    fun cm_symbol s = s
    val ml_structure = Symbol.strSymbol
    val ml_signature = Symbol.sigSymbol
    val ml_functor = Symbol.fctSymbol
    val ml_funsig = Symbol.fsigSymbol

    fun alias (f: pathname) = ()
    fun group (p: perms, e: exports, m: members) = ()
    fun library (p: perms, e: exports, m: members) = ()

    local
	val isMember = StringSet.member
	fun sanity ({ required, granted }, s, error) =
	    if isMember (required, s) orelse isMember (granted, s) then
		error ("duplicate permission name: " ^ s)
	    else ()
    in
	val initialPerms = { required = StringSet.empty,
			     granted = StringSet.empty }
	fun require (a as ({ required, granted }, s, _)) =
	    (sanity a;
	     { required = StringSet.add (required, s), granted = granted })
	fun grant (a as ({ required, granted }, s, _)) =
	    (sanity a;
	     { required = required, granted = StringSet.add (granted, s) })
    end

    fun emptyMembers env = MemberCollection.empty
    fun member (f, c) env = MemberCollection.expandOne (f, c)
    fun members (m1, m2) env = let
	val c1 = m1 env
	val c2 = m2 (MemberCollection.envOf c1)
    in
	MemberCollection.sequential (c1, c2)
    end
    fun guarded_members (c, (m1, m2)) env =
	if saveEval (c, env) then m1 env else m2 env
    fun error_member m env = raise ExplicitError m

    fun emptyExports env = SymbolSet.empty
    fun export s env = SymbolSet.singleton s
    fun exports (e1, e2) env = SymbolSet.union (e1 env, e2 env)
    fun guarded_exports (c, (e1, e2)) env =
	if saveEval (c, env) then e1 env else e2 env
    fun error_export m env = raise ExplicitError m

    fun number i _ = i
    fun variable v e = MemberCollection.num_look e v
    fun plus (e1, e2) e = e1 e + e2 e
    fun minus (e1, e2) e = e1 e - e2 e
    fun times (e1, e2) e = e1 e * e2 e
    fun divide (e1, e2) e = e1 e div e2 e
    fun modulus (e1, e2) e = e1 e mod e2 e
    fun negate ex e = ~(ex e)

    fun ml_defined s e = MemberCollection.ml_look e s
    fun cm_defined s e = MemberCollection.cm_look e s
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

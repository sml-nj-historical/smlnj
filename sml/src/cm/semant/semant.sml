signature CM_SEMANT = sig

    type pathname
    type ml_symbol
    type cm_symbol

    type group

    type perm
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    val file_native : string * pathname -> pathname
    val file_standard : string * pathname -> pathname
    val cm_symbol : string -> cm_symbol
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol

    val alias : pathname -> group
    val group : perm list * exports * members -> group
    val library : perm list * exports * members -> group

    val require : cm_symbol -> perm
    val grant : cm_symbol -> perm

    val emptyMembers : members
    val member : pathname * cm_symbol option -> members
    val members : members * members -> members
    val guarded_members : exp * (members * members) -> members

    val emptyExports : exports
    val export : ml_symbol -> exports
    val exports : exports * exports -> exports
    val guarded_exports : exp * (exports * exports) -> exports

    val number : int -> aexp
    val variable : cm_symbol -> aexp
    val plus : aexp * aexp -> aexp
    val minus : aexp * aexp -> aexp
    val times : aexp * aexp -> aexp
    val divide : aexp * aexp -> aexp
    val modulus : aexp * aexp -> aexp
    val negate : aexp -> aexp

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

    type pathname = AbsPath.t
    type ml_symbol = unit
    type cm_symbol = string

    type group = unit

    type environment = unit
    fun num_look () _ = 0
    fun ml_look () _ = false
    fun cm_look () _ = false

    datatype perm =
	REQUIRE of cm_symbol
      | GRANT of cm_symbol

    type aexp = environment -> int
    type exp = environment -> bool
    type members = unit
    type exports = unit

    fun file_native (s, d) = AbsPath.native { context = d, spec = s }
    fun file_standard (s, d) = AbsPath.standard { context = d, spec = s }
    fun cm_symbol s = s
    fun ml_structure (s: string) = ()
    fun ml_signature (s: string) = ()
    fun ml_functor (s: string) = ()
    fun ml_funsig (s: string) = ()

    fun alias (f: pathname) = ()
    fun group (p: perm list, e: exports, m: members) = ()
    fun library (p: perm list, e: exports, m: members) = ()

    val require = REQUIRE
    val grant = GRANT

    val emptyMembers = ()
    fun member (f: pathname, c: cm_symbol option) = ()
    fun members (m1: members, m2: members) = ()
    fun guarded_members (c: exp, (m1: members, m2: members)) = ()

    val emptyExports = ()
    fun export (s: ml_symbol) = ()
    fun exports (e1: exports, e2: exports) = ()
    fun guarded_exports (c: exp, (e1: exports, e2: exports)) = ()

    fun number i _ = i
    fun variable v e = num_look e v
    fun plus (e1, e2) e = e1 e + e2 e
    fun minus (e1, e2) e = e1 e - e2 e
    fun times (e1, e2) e = e1 e * e2 e
    fun divide (e1, e2) e = e1 e div e2 e
    fun modulus (e1, e2) e = e1 e mod e2 e
    fun negate ex e = ~(ex e)

    fun ml_defined s e = ml_look e s
    fun cm_defined s e = cm_look e s
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

signature CM_SEMANT = sig

    exception ExplicitError of string

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

    val file_native : string * pathname -> pathname
    val file_standard : string * pathname -> pathname
    val cm_symbol : string -> cm_symbol
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol

    val alias : pathname -> group
    val group : perms * exports * members -> group
    val library : perms * exports * members -> group

    val initialPerms : perms
    val require : perms * cm_symbol * complainer -> perms
    val grant : perms * cm_symbol * complainer -> perms

    val emptyMembers : members
    val member : pathname * cm_symbol option -> members
    val members : members * members -> members
    val guarded_members : exp * (members * members) -> members
    val error_member : string -> members

    val emptyExports : exports
    val export : ml_symbol -> exports
    val exports : exports * exports -> exports
    val guarded_exports : exp * (exports * exports) -> exports
    val error_export : string -> exports

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

    exception ExplicitError of string

    type pathname = AbsPath.t
    type ml_symbol = ModName.t
    type cm_symbol = string

    type group = unit

    type environment = unit
    fun num_look () _ = 0
    fun ml_look () _ = false
    fun cm_look () _ = false

    type perms = { required : StringSet.set, granted : StringSet.set }

    type aexp = environment -> int
    type exp = environment -> bool
    type members = unit
    type exports = environment -> ModName.set

    type complainer = string -> unit

    fun file_native (s, d) = AbsPath.native { context = d, spec = s }
    fun file_standard (s, d) = AbsPath.standard { context = d, spec = s }
    fun cm_symbol s = s
    val ml_structure = ModName.structMN
    val ml_signature = ModName.sigMN
    val ml_functor = ModName.functMN
    val ml_funsig = ModName.funsigMN

    fun alias (f: pathname) = ()
    fun group (p: perms, e: exports, m: members) = ()
    fun library (p: perms, e: exports, m: members) = ()

    local
	val member = StringSet.member
	fun sanity ({ required, granted }, s, error) =
	    if member (required, s) orelse member (granted, s) then
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

    val emptyMembers = ()
    fun member (f: pathname, c: cm_symbol option) = ()
    fun members (m1: members, m2: members) = ()
    fun guarded_members (c: exp, (m1: members, m2: members)) = ()
    fun error_member (m: string) = ()

    fun emptyExports env = ModName.empty
    fun export s env = ModName.singleton s
    fun exports (e1, e2) env = ModName.union (e1 env, e2 env)
    fun guarded_exports (c, (e1, e2)) env = if c env then e1 env else e2 env
    fun error_export m env = raise ExplicitError m

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

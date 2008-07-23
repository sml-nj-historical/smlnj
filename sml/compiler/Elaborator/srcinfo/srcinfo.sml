structure SrcInfo : sig
    val merge: { pathname: string, localdb: string } -> unit
end =
struct
    fun merge { pathname, localdb } = () (* !!! placeholder *)
end

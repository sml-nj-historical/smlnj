structure SrcInfo : sig
    val merge: { pathname: string, localdb: string } -> unit
end =
struct
    fun merge { pathname, localdb } = 
	Ens_var2.merge_pickle pathname localdb
end

structure Servers :> SERVERS = struct
    fun add _ = raise Fail "compute servers not available"
    fun start _ = ()
    fun compile _ = false
end

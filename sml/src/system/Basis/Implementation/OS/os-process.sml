structure OS_Process : OS_PROCESS = struct
    type status = int
    val success = 0
    val failure = 1
    fun isSuccess s = s = 0
    val system = SMLBasis.osSystem
    val atExit = AtExit.atExit
    fun terminate s = (SMLBasis.exit s; raise Fail "cannot happen")
    fun exit s = (CleanUp.clean CleanUp.AtExit; terminate s)
    val getEnv = SMLBasis.getEnv
    fun sleep (Time.TIME t) = SMLBasis.osSleep t
end

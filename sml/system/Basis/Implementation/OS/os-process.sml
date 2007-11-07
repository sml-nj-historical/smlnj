(* os-process.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 * All rights reserved.
 *
 * Generic implementation of OS.Process
 *)

structure OS_Process : OS_PROCESS = 
  struct

    type status = int
    val success = 0
    val failure = 1
    fun isSuccess s = s = 0
    val system = SMLBasis.osSystem
    val atExit = AtExit.atExit
    fun terminate s = (SMLBasis.exitProc s; raise Fail "cannot happen")
    fun exit s = (CleanUp.clean CleanUp.AtExit; terminate s)
    val getEnv = SMLBasis.getEnv
    val sleep = SMLBasis.osSleep o TimeImp.toTime_t

  end

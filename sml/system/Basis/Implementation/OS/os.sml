(* os.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 * All rights reserved.
 *
 * Generic OS interface
 *
 *)
structure OSImp : OS = struct

    open OS

    exception SysErr = Assembly.SysErr

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process = OS_Process
    structure IO = OS_IO

    fun errorMsg e = SMLBasis.errorMessage (Int32Imp.fromInt e)
    fun errorName e = SMLBasis.errorName (Int32Imp.fromInt e)
    fun syserror s = SMLBasis.syserror s

end

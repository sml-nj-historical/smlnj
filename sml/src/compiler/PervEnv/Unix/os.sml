(* os.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Generic OS interface (NEW BASIS)
 *
 *)

structure OS : OS =
  struct

    open OS (* open type-only structure to get types *)

    exception SysErr = Assembly.SysErr

    val errorMsg = Posix.Error.errorMsg
    val errorName = Posix.Error.errorName
    val syserror = Posix.Error.syserror

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process = OS_Process
    structure IO = OS_IO

  end (* OS *)

(*
 * $Log: os.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:00  george
 * Version 110.5
 *
 *)

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

    fun errorName _ = "<OS.errorName unimplemented>"
    fun errorMessage _ = "<OS.errorMessage unimplemented>"

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process = OS_Process
    structure IO = OS_IO

  end (* OS *)

(*
 * $Log$
 *)

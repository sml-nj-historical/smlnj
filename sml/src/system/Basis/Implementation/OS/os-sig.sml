(* os-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from os.mldoc (v. 1.4; 1996-04-19)
 *)

signature OS =
  sig
    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO
    eqtype syserror
    exception SysErr of string * syserror option
    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror  : string -> syserror option
    
  end

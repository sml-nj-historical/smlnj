(* posix-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from posix.mldoc (v. 1.4; 1997-10-04)
 *)

signature POSIX =
  sig
    structure Error : POSIX_ERROR
    structure Signal : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
    structure ProcEnv : POSIX_PROC_ENV
    structure FileSys : POSIX_FILE_SYS
    structure IO : POSIX_IO
    structure SysDB : POSIX_SYS_DB
    structure TTY : POSIX_TTY
    
  end

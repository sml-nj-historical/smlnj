(* cinterface.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature CINTERFACE =
  sig

    exception CFunNotFound of string
    val c_function : string -> string -> ('a -> 'b)
	(* the operation for binding ML callable C functions; raises
	 * the exception CFunNotFound if the function is unknown.
	 *)

    type c_function
    val bindCFun : (string * string) -> c_function

  (* utilities for working with system constants *)
    type system_const = (int * string)

    exception SysConstNotFound of string

    val findSysConst : (string * system_const list) -> system_const option
    val bindSysConst : (string * system_const list) -> system_const

  end


(*
 * $Log: cinterface.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)

(* cinterface-sig.sml
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
 * $Log: cinterface-sig.sml,v $
 * Revision 1.2  1997/09/12 18:01:17  jhr
 *   Revealed the bindCFun function.
 *
# Revision 1.1  1997/06/30  19:36:38  jhr
#   Removed System structure; added Unsafe structure.
#
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* sympaths.sig *)

signature SYMPATH =
sig
  datatype path = SPATH of Symbol.symbol list
  val empty : path
  val null : path -> bool
  val extend : path * Symbol.symbol -> path
  val prepend : Symbol.symbol * path -> path
  val append : path * path -> path
  val first : path -> Symbol.symbol
  val rest : path -> path
  val length : path -> int
  val last : path -> Symbol.symbol
  val equal : path * path -> bool
  val toString : path -> string
end

signature INVPATH =
sig
  datatype path = IPATH of Symbol.symbol list
  val empty : path
  val null : path -> bool
  val extend: path * Symbol.symbol -> path
  val append: path * path -> path
  val last: path -> Symbol.symbol
  val lastPrefix: path -> path
  val equal : path * path -> bool
  val toString : path -> string
end

signature CONVERTPATHS =
sig
  type spath
  type ipath
  val invertSPath : spath -> ipath
  val invertIPath : ipath -> spath
end


(*
 * $Log: sympaths.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:35  george
 * Version 110.5
 *
 *)

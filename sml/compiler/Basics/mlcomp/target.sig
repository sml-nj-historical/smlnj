(* target.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TARGET =
  sig

  (* the default size of the "int" type *)
    val defaultIntSz : int

  (* the default size of the "real" type *)
    val defaultRealSz : int

  (* true for 64-bit targets *)
    val is64 : bool

  end

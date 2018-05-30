(* target64.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Definition of TARGET for 64-bit targets
 *)

structure Target : TARGET =
  struct

    val mlValueSz = 64
    val defaultIntSz = 63
    val defaultRealSz = 64
    val is64 = true

  end

(* target32.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Definition of TARGET for 32-bit targets
 *)

structure Target : TARGET =
  struct

    val mlValueSz = 32
    val defaultIntSz = 31
    val defaultRealSz = 64
    val is64 = false

  end

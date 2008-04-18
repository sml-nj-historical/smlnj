(* json-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONParser : sig

    val parse : TextIO.instream -> JSON.value

  end = struct

    fun parse inS = raise Fail "unimplemented"

  end


(* parser.sig
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature (*SMLNJ_PARSER*) MLPARSER = sig

    datatype parseResult
      = EOF
      | ERROR
      | ABORT
      | PARSE of Ast.dec

    val parse : Source.inputSource -> unit -> parseResult

  end

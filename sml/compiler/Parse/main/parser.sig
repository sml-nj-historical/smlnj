(* parser.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature MLPARSER = sig

    datatype parseResult =
	EOF   
      | ERROR 
      | ABORT 
      | PARSE of Ast.dec

    val parse : Source.inputSource -> unit -> parseResult
end

(* COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies *)
(* smlfile.sml *)

signature SMLFILE = sig
    exception Compile of string
    val parseOne : Source.inputSource -> unit -> Ast.dec option
    val parse : Source.inputSource -> Ast.dec
end

structure SmlFile :> SMLFILE = struct

    exception Compile of string

    structure FE = FrontEnd

    val parsePhase = Stats.makePhase "Compiler 010 parse"

    fun fail s = raise (Compile s)

    fun parseOne source = let
	val parser = FE.parse source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun doit () =
	    case parser () of
		FE.EOF => NONE
	      | FE.ABORT => fail "syntax error"
	      | FE.ERROR => fail "syntax error"
	      | FE.PARSE ast => SOME ast
    in
	doit
    end

    fun parse source = let
	val parser = FE.parse source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun loop asts = 
	    case parser () of
		FE.EOF => Ast.SeqDec(rev asts)
	      | FE.ABORT => fail "syntax error"
	      | FE.ERROR => fail "syntax error"
	      | FE.PARSE ast => loop(ast::asts)
    in
	loop nil
    end
end

(* COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies *)
(* smlfile.sml *)

signature SMLFILE = sig
    val parseOne : Source.inputSource -> unit -> Ast.dec option
    val parse : Source.inputSource -> Ast.dec
end

structure SmlFile :> SMLFILE = struct

    structure P = MLParser

    val parsePhase = Stats.makePhase "Compiler 010 parse"

    fun fail s = raise (CompileExn.Compile s)

    fun parseOne source = let
	val parser = P.parse source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun doit () =
	    case parser () of
		P.EOF => NONE
	      | P.ABORT => fail "syntax error"
	      | P.ERROR => fail "syntax error"
	      | P.PARSE ast => SOME ast
    in
	doit
    end

    fun parse source = let
	val parser = P.parse source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun loop asts = 
	    case parser () of
		P.EOF => Ast.SeqDec(rev asts)
	      | P.ABORT => fail "syntax error"
	      | P.ERROR => fail "syntax error"
	      | P.PARSE ast => loop(ast::asts)
    in
	loop nil
    end
end

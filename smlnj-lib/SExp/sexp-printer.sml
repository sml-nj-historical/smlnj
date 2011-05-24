(* sexp-printer.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * A printer for SExp values.
 *)

structure SExpPrinter : sig

    val print : TextIO.outstream * SExp.value -> unit

  end = struct

    structure S = SExp
    structure F = Format

    fun print (strm, sexp) = let
	  fun pr s = TextIO.output(strm, s)
	  fun prList [] = pr ("()")
	    | prList [v] = (pr "("; prVal v; pr ")")
	    | prList (v::vs) = (
		pr "("; prVal v; List.app (fn v => (pr " "; prVal v)) vs; pr ")")
	  and prVal (S.LIST value) = prList (value)
	    | prVal (S.SYMBOL value) = pr (Atom.toString value)
	    | prVal (S.BOOL value) = pr (if value then "#t" else "#f")
	    | prVal (S.INT value) = pr (F.format "%d" [F.LINT value])
	    | prVal (S.FLOAT value) = pr (F.format "%g" [F.REAL value])
	    | prVal (S.STRING value) = pr (concat ["\"", String.toString value, "\""])
	  in
	    prVal sexp
	  end

  end

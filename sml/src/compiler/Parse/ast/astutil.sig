(* Copyright 1992 by AT&T Bell Laboratories 
 *
 *)
signature ASTUTIL =
  sig

    val checkFix : int * ErrorMsg.complainer -> int

    (* BUILDS VARIOUS CONSTRUCTIONS *)
    val makeSEQdec : Ast.dec * Ast.dec -> Ast.dec

    val layered : Ast.pat * Ast.pat * ErrorMsg.complainer -> Ast.pat

    (* SYMBOLS *)
    val arrowTycon : Symbol.symbol
    val bogusID : Symbol.symbol
    val exnID : Symbol.symbol
    val symArg : Symbol.symbol
    val itsym : Symbol.symbol list

    val unitExp : Ast.exp
    val unitPat : Ast.pat

    (* QUOTES *)
    val QuoteExp : string -> Ast.exp
    val AntiquoteExp : Ast.exp -> Ast.exp

end  (* signature ASTUTIL *)


(*
 * $Log: astutil.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:19  george
 * Version 110.5
 *
 *)

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
 * Revision 1.4  1997/11/24 20:22:45  dbm
 *   Eliminate resultId, returnId and Ast translation functions.
 *
 * Revision 1.3  1997/09/23  03:58:29  dbm
 *   Added transforms for EntityEnv.Unbound fix (bugs 1270, 1271).
 *
 * Revision 1.2  1997/01/28  23:20:43  jhr
 * Integer and word literals are now represented by IntInf.int (instead of
 * as strings).
 *
 * Revision 1.1.1.1  1997/01/14  01:38:43  george
 *   Version 109.24
 *
 *)

(* 
 * burg-ast.sml
 *
 * Abstract syntax trees for BURG specifications.
 *
 * $Log$
 * Revision 1.1.1.5  1999/04/17 16:22:21  monnier
 * version 110.11
 *
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:28  george
 *   ml-burg Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)

structure BurgAST =
  struct

    datatype decl_ast = START of string
		      | TERM of (string * string option) list
		      | TERMPREFIX of string
		      | RULEPREFIX of string
		      | SIG of string

    datatype pattern_ast = PAT of (string * pattern_ast list)

    datatype rule_ast = RULE of (string * pattern_ast * string * int list)

    datatype spec_ast = SPEC of {head : string list,
				 decls : decl_ast list, 
				 rules : rule_ast list,
				 tail : string list}
  end (* BurgAST *)


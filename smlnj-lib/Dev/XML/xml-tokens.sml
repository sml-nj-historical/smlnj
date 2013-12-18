(* xml-tokens.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure XMLTokens =
  struct

    datatype token
      = EOF
      | OPEN_START_TAG		(* "<" *)
      | OPEN_END_TAG		(* "</" *)
      | OPEN_XML_TAG		(* "<?xml" *)
      | CLOSE_XML_TAG		(* "?>" *)
      | CLOSE_TAG		(* ">" *)
      | CLOSE_EMPTY_TAG		(* "/>" *)
      | SYM_EQ			(* "=" inside a tag *)
      | ID of string		(* element or attribute name *)
      | LIT of string		(* quoted attribute value *)
    (* the following tags are content *)
      | TEXT of string		(* non-whitespace/non-comment text *)
      | WS of string		(* whitespace *)
      | COM of string		(* XML comment; string does not include "<!--" and "-->" *)
      | CDATA of string		(* CDATA text; string does not include "<![CDATA[" and "]]>" *)

  end

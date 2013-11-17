(* xml-tokens.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure XMLTokens =
  struct

    datatype token
      = EOF
      | OPEN_START_TAG
      | OPEN_END_TAG
      | OPEN_XML_TAG
      | CLOSE_XML_TAG
      | CLOSE_TAG
      | CLOSE_EMPTY_TAG
      | SYM_EQ
      | ID of string
      | LIT of string
      | TEXT of string
      | WS of string
      | CDATA of string

  end

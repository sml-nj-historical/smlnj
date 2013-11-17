(* xml-schema-sig.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Information about an XML schema (or DTD) that is used in the implementation of
 * a parser.
 *)

signature XML_SCHEMA =
  sig

    type element
    type attribute

  (* create an element; returns NONE if the element name is unrecognized *)
    val element : string -> element option

  (* should leading and trailing whitespace be preserved in the content of this element? *)
    val preserveWS : element -> bool

  (* equality test *)
    val same : element * element -> bool

    val toString : element -> string

  (* create an attribute from a name/value pair *)
    val attribute : (string * string) -> attribute

  end

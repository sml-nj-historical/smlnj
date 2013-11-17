(* xml-tree-sig.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A tree representation of an XML file.
 *)

signature XML_TREE =
  sig

    structure Schema : XML_SCHEMA

    datatype content
      = TEXT of string
      | CDATA of string
      | ELEMENT of {
	    name : Schema.element,
	    attrs : Schema.attribute list,
	    content : content list
	  }

    type tree = {
	xmlDecl : Schema.attribute list,	(* empty if there is no decl *)
	content : content
      }

  end

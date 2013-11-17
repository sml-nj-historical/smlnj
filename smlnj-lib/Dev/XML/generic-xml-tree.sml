(* generic-xml-tree.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenericXMLTree =
  struct
    local
      structure Schema =
	struct

	  type element = Atom.atom
	  type attribute = (Atom.atom * string)

	(* create an element; returns NONE if the element name is unrecognized *)
	  val element = SOME o Atom.atom

	(* should leading and trailing whitespace be preserved in the content of this element? *)
	  fun preserveWS _ = false

	(* equality test *)
	  val same = Atom.same

	  val toString = Atom.toString

	(* create an attribute from a name/value pair *)
	  fun attribute (id, value) = (Atom.atom id, value)

	end
      structure Tree = XMLTreeFn (Schema)
    in
    open Tree
  end

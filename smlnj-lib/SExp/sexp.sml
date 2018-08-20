(* sexp.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * This is the tree representation of a SExp data as produced/consumed
 * by the tree parser.
 *)

structure SExp =
  struct

    datatype value
      = SYMBOL of Atom.atom
      | LIST of value list
      | BOOL of bool
      | INT of IntInf.int
      | FLOAT of real
      | STRING of string

    fun same (SYMBOL a, SYMBOL b) = Atom.same (a, b)
      | same (LIST a, LIST b) = ListPair.allEq same (a, b)
      | same (BOOL a, BOOL b) = (a = b)
      | same (INT a, INT b) = (a = b)
      | same (FLOAT a, FLOAT b) = Real.==(a, b)
      | same (STRING a, STRING b) = (a = b)
      | same _ = false

  end

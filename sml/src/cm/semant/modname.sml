(*
 * semant/modname.sml:
 *  `module name' abstraction and related types
 *
 *   Copyright (c) 1999 by Lucent Technologies
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure ModName : MODNAME = struct

    structure Symbol = GenericVC.Symbol

    type symbol = Symbol.symbol

    type namespace = Symbol.namespace

    exception ModuleNameError and PathError

    type t = symbol
    type path = t list
    type set = t Set.set

    val equal = Symbol.eq
    val namespaceOf = Symbol.nameSpace
    fun symbolOf n = n
    fun nameOf n = Symbol.name n

    val lt = Symbol.symbolCMLt

    fun makestring mn =
	(case Symbol.nameSpace mn of
	     Symbol.STRspace => "structure "
	   | Symbol.SIGspace => "signature "
	   | Symbol.FCTspace => "functor "
	   | Symbol.FSIGspace => "functor signature "
	   | _ => raise ModuleNameError)
	^ (Symbol.name mn)

    (* we don't really have to check the name space if we trust the parser *)
    fun ofSymbol sy = sy

    fun filterSymbols sl = let
	fun filt (sy, l) =
	    case Symbol.nameSpace sy of
		Symbol.STRspace => sy :: l
	      | Symbol.SIGspace => sy :: l
	      | Symbol.FCTspace => sy :: l
	      | Symbol.FSIGspace => sy :: l
	      | _ => l
    in
	foldr filt [] sl
    end

    val STRspace = Symbol.STRspace
    val SIGspace = Symbol.SIGspace
    val FCTspace = Symbol.FCTspace
    val FSIGspace = Symbol.FSIGspace

    val structMN = Symbol.strSymbol
    val sigMN = Symbol.sigSymbol
    val functMN = Symbol.fctSymbol
    val funsigMN = Symbol.fsigSymbol

    fun create (ns, n) =
	(case ns of
	     Symbol.STRspace => structMN
	   | Symbol.SIGspace => sigMN
	   | Symbol.FCTspace => functMN
	   | Symbol.FSIGspace => funsigMN
	   | _ => raise ModuleNameError) n

    fun pathFirstModule [] = raise PathError
      | pathFirstModule (h :: _) = h

    fun restOfPath [] = NONE
      | restOfPath [_] = NONE
      | restOfPath (_ :: t) = SOME t

    fun pathLastModule [] = raise PathError
      | pathLastModule [m] = m
      | pathLastModule (_ :: t) = pathLastModule t

    fun pathOfSymbolList sl = sl
    fun mnListOfPath p = p
    fun pathOfMNList l = l

    fun createPathSML (sl, x) = foldl (fn (s, p) => (structMN s) :: p) [x] sl

    fun nameOfPath [] = raise PathError
      | nameOfPath [m] = nameOf m
      | nameOfPath (h :: t) = (nameOf h) ^ "." ^ (nameOfPath t)


    val { memberOf, union, intersection, difference, add, addl,
	  makeset, eq = sameSet, ... } =
	Set.gen { eq = equal, lt = lt }

    val fold = Set.fold
    val empty = Set.empty
    val isEmpty = Set.isEmpty
    val makelist = Set.makelist
    val singleton = Set.singleton

end

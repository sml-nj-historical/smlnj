(*
 * Sets of symbols.
 *   Hooks into compiler and uses SML/NJ library implementation of sets.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure SymbolSet = BinarySetFn
    (struct
	type ord_key = GenericVC.Symbol.symbol
	fun compare (s1, s2) =
	    if GenericVC.Symbol.symbolCMLt (s1, s2) then LESS
	    else if GenericVC.Symbol.eq (s1, s2) then EQUAL
	    else GREATER
    end)

(* bindings.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Bindings : BINDINGS =
  struct

    structure S  = Symbol
    structure T  = Types
    structure V  = VarCon
    structure M =  Modules

    datatype binding
      = VALbind of V.var
      | CONbind of V.datacon
      | TYCbind of T.tycon
      | SIGbind of M.Signature
      | STRbind of M.Structure
      | FSGbind of M.fctSig
      | FCTbind of M.Functor
      | FIXbind of Fixity.fixity

  (* used for statenv sorting in env/statenv.sml *)
    fun binderGt ((s1, rb1), (s2, rb2)) = let
	(* hopefully the following gets optimized into an identity function
	 * on tags... *)
	  fun bnum (VALbind _) = 0
	    | bnum (CONbind _) = 1
	    | bnum (TYCbind _) = 2
	    | bnum (SIGbind _) = 3
	    | bnum (STRbind _) = 4
	    | bnum (FSGbind _) = 5
	    | bnum (FCTbind _) = 6
	    | bnum (FIXbind _) = 7
	  in
	    case Int.compare (bnum rb1, bnum rb2)
	     of EQUAL => S.symbolGt (s1, s2)
	      | GREATER => true
	      | LESS => false
	  end

  end (* structure Bindings *)

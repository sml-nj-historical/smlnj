(*
 * SML source skeletons.
 *   (This has been vastly streamlined and simplified.)
 *
 *   Copyright (c) 1999 by Bell Laboratories, Lucent Technologies
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Skeleton = struct

    type symbol = Symbol.symbol
    type sympath = GenericVC.SymPath.path

    datatype decl =
	Bind of symbol * modExp
      | Local of decl * decl
      | Par of decl list
      | Seq of decl list
      | Open of modExp
      | Ref of SymbolSet.set

    and modExp =
	Var of sympath
      | Decl of decl
      | Let of decl * modExp
      | Ign1 of modExp * modExp
end

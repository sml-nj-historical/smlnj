(*
 * SML source skeletons.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Skeleton = struct

    type symbol = GenericVC.Symbol.symbol
    type sympath = GenericVC.SymPath.path

    datatype decl =
	StrDecl of { name: symbol,
		     def: strExp,
		     constraint: strExp option } list    
      | FctDecl of { name: symbol, def: fctExp } list    
      | LocalDecl of decl * decl
      | SeqDecl of decl list    
      | OpenDecl of strExp list
      | DeclRef of SymbolSet.set

    and strExp = 
	VarStrExp of sympath
      | BaseStrExp of decl
      | AppStrExp of sympath * strExp list
      | LetStrExp of decl * strExp  
      | AugStrExp of strExp * SymbolSet.set
      | ConStrExp of strExp * strExp

    and fctExp = 
	VarFctExp of sympath * fctExp option 
      | BaseFctExp of { params: (symbol option * strExp) list,
		        body: strExp,
			constraint: strExp option }
      | AppFctExp of sympath * strExp list * fctExp option
      | LetFctExp of decl * fctExp
end

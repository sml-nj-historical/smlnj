(*
 * Get the toplevel exports from a skeleton.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SKELEXPORTS = sig
    val exports : Skeleton.decl -> SymbolSet.set
end

structure SkelExports :> SKELEXPORTS = struct

    structure SK = Skeleton
    structure SS = SymbolSet

    fun exports d = let
	fun e (SK.StrDecl l, a) = SS.addList (a, map #name l)
	  | e (SK.FctDecl l, a) = SS.addList (a, map #name l)
	  | e (SK.LocalDecl (l, b), a) = e (b, a)
	  | e (SK.SeqDecl l, a) = foldl e a l
	  | e (SK.OpenDecl _, a) = a	(* cannot happen *)
	  | e (SK.DeclRef _, a) = a
    in
	e (d, SS.empty)
    end
end

(*
 * User-visible ML-side representation of certain primitive C types.
 *
 * x86/Sparc/PPC version (all ints: 32 bit, all floats: 64 bit)
 *
 *  (C) 2004 The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure MLRep = struct
    structure Signed = Int32
    structure Unsigned = Word32
    structure Real = Real64

    (* word-style bit-operations on integers... *)
    structure SignedBitops = IntBitOps
				 (structure I = Signed structure W = Unsigned)
end

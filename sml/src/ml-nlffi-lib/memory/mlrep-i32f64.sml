(*
 * User-visible ML-side representation of certain primitive C types.
 *
 * x86/Sparc version (all ints: 32 bit, all floats: 64 bit)
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure MLRep = struct
    structure Signed = Int32
    structure Unsigned = Word32
    structure Real = Real64

    (* word-style bit-operations on integers... *)
    structure SignedBitops = IntBitOps
				 (structure I = Signed structure W = Unsigned)
end

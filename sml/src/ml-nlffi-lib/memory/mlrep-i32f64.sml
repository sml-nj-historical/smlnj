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
    structure SChar = Int32
    structure UChar = Word32
    structure SInt = Int32
    structure UInt = Word32
    structure SShort = Int32
    structure UShort = Word32
    structure SLong = Int32
    structure ULong = Word32
    structure Float = Real
    structure Double = Real

    (* word-style bit-operations on integers... *)
    structure SCharBitops = IntBitOps
				(structure I = SChar structure W = UChar)
    structure SIntBitops = IntBitOps
			       (structure I = SInt structure W = UInt)
    structure SShortBitops = IntBitOps
				 (structure I = SShort structure W = UShort)
    structure SLongBitops = IntBitOps
				(structure I = SLong structure W = ULong)
end

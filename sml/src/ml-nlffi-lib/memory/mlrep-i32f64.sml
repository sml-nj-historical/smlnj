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
end

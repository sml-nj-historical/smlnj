(*
 * Primitives for "raw" memory access.
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature CMEMORY = sig
    type addr
    val null : addr
    val isNull : addr -> bool
    val ++ : addr * int -> addr
    val -- : addr * addr -> int
    val compare : addr * addr -> order
    val bcopy : { from: addr, to: addr, bytes: word } -> unit
    val alloc : word -> addr option
    val free : addr -> unit

    (* actual sizes of C types (not their ML representations) in bytes *)
    val addr_size : word
    val char_size : word
    val short_size : word
    val int_size : word
    val long_size : word
    val float_size : word
    val double_size : word

    (* fetching from memory *)
    val load_addr : addr -> addr
    val load_schar : addr -> MLRep.SChar.int
    val load_uchar : addr -> MLRep.UChar.word
    val load_sshort : addr-> MLRep.SShort.int
    val load_ushort : addr-> MLRep.UShort.word
    val load_sint : addr -> MLRep.SInt.int
    val load_uint : addr -> MLRep.UInt.word
    val load_slong : addr -> MLRep.SLong.int
    val load_ulong : addr -> MLRep.ULong.word
    val load_float : addr -> MLRep.Float.real
    val load_double : addr -> MLRep.Double.real

    (* storing into memory *)
    val store_addr : addr * addr -> unit
    val store_schar : addr * MLRep.SChar.int -> unit
    val store_uchar : addr * MLRep.UChar.word -> unit
    val store_sshort : addr * MLRep.SShort.int -> unit
    val store_ushort : addr * MLRep.UShort.word -> unit
    val store_sint : addr * MLRep.SInt.int -> unit
    val store_uint : addr * MLRep.UInt.word -> unit
    val store_slong : addr * MLRep.SLong.int -> unit
    val store_ulong : addr * MLRep.ULong.word -> unit
    val store_float : addr * MLRep.Float.real -> unit
    val store_double : addr * MLRep.Double.real -> unit

    val int_bits : word

    (* types used in C calling convention *)
    type cc_addr
    type cc_schar
    type cc_uchar
    type cc_sint
    type cc_uint
    type cc_sshort
    type cc_ushort
    type cc_slong
    type cc_ulong
    type cc_float
    type cc_double

    (* wrapping and unwrapping for cc types *)
    val wrap_addr : addr -> cc_addr
    val wrap_schar : MLRep.SChar.int -> cc_schar
    val wrap_uchar : MLRep.UChar.word -> cc_uchar
    val wrap_sint : MLRep.SInt.int -> cc_sint
    val wrap_uint : MLRep.UInt.word -> cc_uint
    val wrap_sshort : MLRep.SShort.int -> cc_sshort
    val wrap_ushort : MLRep.UShort.word -> cc_ushort
    val wrap_slong : MLRep.SLong.int -> cc_slong
    val wrap_ulong : MLRep.ULong.word -> cc_ulong
    val wrap_float : MLRep.Float.real -> cc_float
    val wrap_double : MLRep.Double.real -> cc_double

    val unwrap_addr : cc_addr -> addr
    val unwrap_schar : cc_schar -> MLRep.SChar.int
    val unwrap_uchar : cc_uchar -> MLRep.UChar.word
    val unwrap_sint : cc_sint -> MLRep.SInt.int
    val unwrap_uint : cc_uint -> MLRep.UInt.word
    val unwrap_sshort : cc_sshort -> MLRep.SShort.int
    val unwrap_ushort : cc_ushort -> MLRep.UShort.word
    val unwrap_slong : cc_slong -> MLRep.SLong.int
    val unwrap_ulong : cc_ulong -> MLRep.ULong.word
    val unwrap_float : cc_float -> MLRep.Float.real
    val unwrap_double : cc_double -> MLRep.Double.real
end

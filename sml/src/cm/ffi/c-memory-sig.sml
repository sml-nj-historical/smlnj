(*
 * Primitives for "raw" memory access.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CMEMORY = sig
    type addr
    val null : addr
    val isNull : addr -> bool
    val ++ : addr * int -> addr

    type uchar
    type schar
    type ushort
    type sshort
    type uint
    type sint
    type ulong
    type slong
    type float
    type double

    val addr_size : word
    val char_size : word
    val short_size : word
    val int_size : word
    val long_size : word
    val float_size : word
    val double_size : word

    type 'a load_instr = addr -> 'a
    type 'a store_instr = addr * 'a -> unit

    val load_addr : addr load_instr
    val load_uchar : uchar load_instr
    val load_ushort : ushort load_instr
    val load_uint : uint load_instr
    val load_ulong : ulong load_instr
    val load_float : float load_instr
    val load_double : double load_instr

    val store_addr : addr store_instr
    val store_uchar : uchar store_instr
    val store_ushort : ushort store_instr
    val store_uint : uint store_instr
    val store_ulong : ulong store_instr
    val store_float : float store_instr
    val store_double : double store_instr

    val char_u2s : uchar -> schar
    val char_s2u : schar -> uchar
    val short_u2s : ushort -> sshort
    val short_s2u : sshort -> ushort
    val int_u2s : uint -> sint
    val int_s2u : sint -> uint
    val long_u2s : ulong -> slong
    val long_s2u : slong -> ulong

    val >> : uint * word -> uint
    val << : uint * word -> uint
    val andb : uint * uint -> uint
    val orb : uint * uint -> uint
    val notb : uint -> uint
end

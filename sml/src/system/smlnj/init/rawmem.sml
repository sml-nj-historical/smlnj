(*
 * Raw memory access primops and raw C calls.
 * (This is for use by ml-nlffi.)
 *
 *   Copyright (c) 2001 by Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure RawMemInlineT = struct
    val w8l  : word32 -> word32           = InLine.raww8l
    val i8l  : word32 -> int32            = InLine.rawi8l
    val w16l : word32 -> word32           = InLine.raww16l
    val i16l : word32 -> int32            = InLine.rawi16l
    val w32l : word32 -> word32           = InLine.raww32l
    val i32l : word32 -> int32            = InLine.rawi32l
    val f32l : word32 -> real             = InLine.rawf32l
    val f64l : word32 -> real             = InLine.rawf64l
    val w8s  : word32 * word32 -> unit    = InLine.raww8s
    val i8s  : word32 * int32  -> unit    = InLine.rawi8s
    val w16s : word32 * word32 -> unit    = InLine.raww16s
    val i16s : word32 * int32  -> unit    = InLine.rawi16s
    val w32s : word32 * word32 -> unit    = InLine.raww32s
    val i32s : word32 * int32  -> unit    = InLine.rawi32s
    val f32s : word32 * real   -> unit    = InLine.rawf32s
    val f64s : word32 * real   -> unit    = InLine.rawf64s
    val rawccall : word32 * 'a * 'b -> 'c = InLine.rawccall
end

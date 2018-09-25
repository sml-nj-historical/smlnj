(* rawmem.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Raw memory access primops and raw C calls.
 * (This is for use by ml-nlffi.)
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)

structure RawMemInlineT =
  struct

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

    (* Allen Leung's additions... *)
    val rawrecord : int -> 'a = InLine.rawrecord
    val rawrecord64 : int -> 'a = InLine.rawrecord64

    val subw8  : 'a * word32 -> word32 = InLine.rawselectw8
    val subi8  : 'a * word32 -> int32  = InLine.rawselecti8
    val subw16 : 'a * word32 -> word32 = InLine.rawselectw16
    val subi16 : 'a * word32 -> int32  = InLine.rawselecti16
    val subw32 : 'a * word32 -> word32 = InLine.rawselectw32
    val subi32 : 'a * word32 -> int32  = InLine.rawselecti32
    val subf32 : 'a * word32 -> real   = InLine.rawselectf32
    val subf64 : 'a * word32 -> real   = InLine.rawselectf64

    val updw8  : 'a * word32 * word32 -> unit = InLine.rawupdatew8
    val updi8  : 'a * word32 * int32  -> unit = InLine.rawupdatei8
    val updw16 : 'a * word32 * word32 -> unit = InLine.rawupdatew16
    val updi16 : 'a * word32 * int32  -> unit = InLine.rawupdatei16
    val updw32 : 'a * word32 * word32 -> unit = InLine.rawupdatew32
    val updi32 : 'a * word32 * int32  -> unit = InLine.rawupdatei32
    val updf32 : 'a * word32 * real   -> unit = InLine.rawupdatef32
    val updf64 : 'a * word32 * real   -> unit = InLine.rawupdatef64

  end (* structure RawMemInlineT *)

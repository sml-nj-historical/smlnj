(* object-desc.sig
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Abstract interface to the encoding of object description headers.
 *)

signature OBJECT_DESC =
  sig

    type tag

    val tagWidth : int (* number of bits to hold a tag *)
    val powTagWidth : int (* 2 ^ tagWidth *)
    val maxLength : int (* one greater than max length value *)

  (* tag values *)
    val tag_record : tag
    val tag_ref : tag
    val tag_vec_hdr : tag
    val tag_vec_data : tag
    val tag_arr_hdr : tag
    val tag_arr_data : tag
    val tag_raw32 : tag
    val tag_raw64 : tag
    val tag_special : tag

  (* build a descriptor from a tag and length *)
    val makeDesc : (int * tag) -> LargeWord.word

  (* fixed descriptors *)
    val desc_pair : LargeWord.word
    val desc_ref : LargeWord.word
    val desc_real64 : LargeWord.word
    val desc_polyvec : LargeWord.word
    val desc_polyarr : LargeWord.word
    val desc_special : LargeWord.word	(* with 0 length *)

  (* length codes for special descriptors *)
    val special_evaled_susp : int
    val special_unevaled_susp : int
    val special_weak : int
    val special_nulled_weak : int

  end;

(*
 * $Log: object-desc.sig,v $
 * Revision 1.2  1998/11/18 03:53:12  jhr
 *  New array representations.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)


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
    val tag_pair : tag
    val tag_array : tag
    val tag_string : tag
    val tag_word8array : tag
    val tag_reald : tag
    val tag_realdarray : tag
    val tag_cont : tag
    val tag_block : tag
    val tag_variant : tag (* currently not used *)
    val tag_special : tag
    val tag_backptr : tag

  (* build a descriptor from a tag and length *)
    val makeDesc : (int * tag) -> LargeWord.word

  (* fixed descriptors *)
    val desc_pair : LargeWord.word
    val desc_reald : LargeWord.word

  (* special descriptors *)
    val desc_special : LargeWord.word
    val special_evaled_susp : int
    val special_unevaled_susp : int
    val special_weak : int
    val special_nulled_weak : int

  end;

(*
 * $Log$
 *)


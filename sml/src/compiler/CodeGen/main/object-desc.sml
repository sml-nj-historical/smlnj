(* object-desc.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * The encoding of object description headers.
 *
 * WARNING: this file must agree with the run-time system values and those
 * used in boot/core.sml and boot/Unsafe/object.sml.
 *)

structure ObjectDesc :> OBJECT_DESC =
  struct

    type tag = word

    structure W = Word
    structure LW = LargeWord

  (* taken from runtime/tags.h *)
    val tagWidth = 7		(* 5 minor tag bits plus 2 major tag bits *)
  (* one greater than the maximum length field value (sign should be 0) *)
    val maxLength = W.toInt(W.<<(0w1, W.-(0w31, W.fromInt tagWidth)))
    val powTagWidth = W.toInt(W.<<(0w1, W.fromInt tagWidth))

  (* tag values *)
    local
      fun mkTag t = W.orb(W.<<(t, 0w2), 0w2)
    in
    val tag_record	= mkTag 0w0
    val tag_vec_hdr	= mkTag 0w1
    val tag_vec_data	= tag_record
    val tag_arr_hdr	= mkTag 0w2
    val tag_arr_data	= mkTag 0w3
    val tag_ref		= tag_arr_data
    val tag_raw32	= mkTag 0w4
    val tag_raw64	= mkTag 0w5
    val tag_special	= mkTag 0w6
    end (* local *)

  (* build a descriptor from a tag and length *)
    fun makeDesc (len, t) = 
	  LW.orb(LW.<<(LW.fromInt len, W.fromInt tagWidth), W.toLargeWord t)

  (* array/vector header codes *)
    val seq_poly	= 0
    val seq_word8	= 1
    val seq_word16	= 2
    val seq_word31	= 3
    val seq_word32	= 4
    val seq_real32	= 5
    val seq_real64	= 6

  (* fixed descriptors *)
    val desc_pair = makeDesc(2, tag_record)
    val desc_ref = makeDesc(1, tag_ref)
    val desc_real64 = makeDesc(2, tag_raw64)
    val desc_polyvec = makeDesc(seq_poly, tag_vec_hdr)
    val desc_polyarr = makeDesc(seq_poly, tag_arr_hdr)
    val desc_special = makeDesc(0, tag_special)

  (* special descriptors *)
    val special_unevaled_susp	= 0
    val special_evaled_susp	= 1
    val special_weak		= 2
    val special_nulled_weak	= 3

  end;


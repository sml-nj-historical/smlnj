(* object-desc-sig.sml
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
    val tagWidth = 6		(* 4 minor tag bits plus 2 major tag bits *)
  (* one greater than the maximum length field value (sign should be 0) *)
    val maxLength = W.toInt(W.<<(0w1, W.-(0w31, W.fromInt tagWidth)))
    val powTagWidth = W.toInt(W.<<(0w1, W.fromInt tagWidth))

  (* tag values *)
    local
      fun tagXLen(lTag, n) = W.orb(lTag, W.<<(n, 0w2))
    in
      fun tagWLen n  = tagXLen(0wx22, n)
      fun tagWOLen n = tagXLen(0wx02, n)		  
    end
    val tag_record		= tagWLen 0w0
    val tag_array		= tagWLen 0w1
    val tag_string		= tagWLen 0w2
    val tag_word8array		= tagWLen 0w4
    val tag_realdarray		= tagWLen 0w5
    val tag_cont                = tagWLen 0w6
    val tag_block               = tagWLen 0w7
    val tag_pair		= tagWOLen 0w0
    val tag_reald		= tagWOLen 0w1
    val tag_variant		= tagWOLen 0w3 (* currently not used *)
    val tag_special		= tagWOLen 0w4
    val tag_backptr		= tagWOLen 0w5

  (* build a descriptor from a tag and length *)
    fun makeDesc (len, t) = 
	  LW.orb(LW.<<(LW.fromInt len, W.fromInt tagWidth), W.toLargeWord t)

  (* fixed descriptors *)
    val desc_pair = makeDesc(2, tag_pair)
    val desc_reald = makeDesc(2, tag_reald)

  (* special descriptors *)
    val desc_special = makeDesc(0, tag_special)
    val special_unevaled_susp	= 0
    val special_evaled_susp	= 1
    val special_weak		= 2
    val special_nulled_weak	= 3

  end;

(*
 * $Log: object-desc.sml,v $
 * Revision 1.2  1998/02/15 19:40:49  jhr
 *   Added CVS Log.
 *
 *)


(* cml-text-stream-io-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1991 John H. Reppy.
 *
 * This extends the SMLBL TEXT_STREAM_IO interface with event-valued operations.
 *)

signature CML_TEXT_STREAM_IO =
  sig
    include TEXT_STREAM_IO

    val input1Evt    : instream -> (elem * instream) option CML.event
    val inputNEvt    : (instream * int) -> (vector * instream) CML.event
    val inputEvt     : instream -> (vector * instream) CML.event
    val inputAllEvt  : instream -> (vector * instream) CML.event
    val inputLineEvt : instream -> vector CML.event

  end;

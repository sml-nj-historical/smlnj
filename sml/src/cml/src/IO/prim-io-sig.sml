(* prim-io-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1991 John H. Reppy.
 *
 * This is the CML equivalent of the SMLBL's PRIM_IO signature.  The
 * differences are that we use event-valued interfaces instead of
 * non-blocking operations, and that the operations are not optional.
 *)

signature PRIM_IO =
  sig

    type 'a event = 'a CML.event

    type array
    type vector
    type elem
    eqtype pos

    val compare : (pos * pos) -> order

    datatype reader = RD of {
	name       : string, 
	chunkSize  : int,
	readVec    : int -> vector,
        readArr    : {buf : array, i : int, sz : int option} -> int,
	readVecEvt : int -> vector event,
	readArrEvt : {buf : array, i : int, sz : int option} -> int event,
	avail      : unit -> int option,
	getPos     : (unit -> pos) option,
	setPos     : (pos -> unit) option,
        endPos     : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close      : unit -> unit,
	ioDesc     : OS.IO.iodesc option
      }

    datatype writer = WR of {
	name        : string,
	chunkSize   : int,
	writeVec    : {buf : vector, i : int, sz : int option} -> int,
	writeArr    : {buf : array, i : int, sz : int option} -> int,
	writeVecEvt : {buf : vector, i : int, sz : int option} -> int event,
	writeArrEvt : {buf : array, i : int, sz : int option} -> int event,
	getPos      : (unit -> pos) option,
	setPos      : (pos -> unit) option,
        endPos      : (unit -> pos) option,
	verifyPos   : (unit -> pos) option,
	close       : unit -> unit,
	ioDesc      : OS.IO.iodesc option
      }

  end;


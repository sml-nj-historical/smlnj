(* prim-io-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1991 John H. Reppy.
 *
 *)

functor PrimIO (

    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
      sharing type Vector.vector = Array.vector
      sharing type Vector.elem = Array.elem
    val someElem : Vector.elem
    eqtype pos
    val compare : (pos * pos) -> order

  ) : PRIM_IO = struct

    type 'a event = 'a CML.event

    structure A = Array
    structure V = Vector

    type elem = A.elem
    type vector = V.vector
    type array = A.array
    type pos = pos

    val compare = compare

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
	verifyPos  : (unit -> pos) option,
	close       : unit -> unit,
	ioDesc      : OS.IO.iodesc option
      }

  end (* PrimIO *)

(* mono-buffer.sig
 *
 * Monomorphic imperative buffers; see
 *
 *   https://github.com/SMLFamily/BasisLibrary/wiki/2018-001-Addition-of-monomorphic-buffers
 *
 * for a description
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MONO_BUFFER =
  sig

    type buf

    type elem		(* buffer element type *)
    type vector		(* vectors of elements *)
    type slice		(* vector-slices of elements *)
    type array		(* mutable arrays of elements *)
    type array_slice	(* array-slices of elements *)

  (* maximum number of elements that a buffer can contain *)
    val maxLen : int

  (* create a new buffer; the argument is a hit as to the requested capacity.
   * Use zero for the default size.
   *)
    val new : int -> buf

  (* get the current contents of the buffer as a vector *)
    val contents : buf -> vector

    val copy : {src : buf, dst : array, di : int} -> unit

    val length : buf -> int

    val sub : buf * int -> elem

    val clear : buf -> unit

    val reset : buf -> unit

    val reserve : buf * int -> unit

    val add1 : buf * elem -> unit
    val addVec : buf * vector -> unit
    val addSlice : buf * slice -> unit
    val addArr : buf * array -> unit
    val addArrSlice : buf * array_slice -> unit

  end
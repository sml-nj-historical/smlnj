(*
 * Getter and setter functions for primitive C types, using ML-side
 * representation types for convenience.
 *
 *   (C) 2002, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature C_GETSET = sig

    (* "fetch" methods for various types;
     * fetching does not care about constness *)
    structure Get : sig

	(* primitive types *)
	val schar :  'c C.schar_obj -> MLRep.SChar.int
	val uchar :  'c C.uchar_obj -> MLRep.UChar.word
	val sint :   'c C.sint_obj -> MLRep.SInt.int
	val uint :   'c C.uint_obj -> MLRep.UInt.word
	val sshort : 'c C.sshort_obj -> MLRep.SShort.int
	val ushort : 'c C.ushort_obj -> MLRep.UShort.word
	val slong :  'c C.slong_obj -> MLRep.SLong.int
	val ulong :  'c C.ulong_obj -> MLRep.ULong.word
	val float :  'c C.float_obj -> MLRep.Float.real
	val double : 'c C.double_obj -> MLRep.Double.real

	(* alt *)
	val schar' :  'c C.schar_obj' -> MLRep.SChar.int
	val uchar' :  'c C.uchar_obj' -> MLRep.UChar.word
	val sint' :   'c C.sint_obj' -> MLRep.SInt.int
	val uint' :   'c C.uint_obj' -> MLRep.UInt.word
	val sshort' : 'c C.sshort_obj' -> MLRep.SShort.int
	val ushort' : 'c C.ushort_obj' -> MLRep.UShort.word
	val slong' :  'c C.slong_obj' -> MLRep.SLong.int
	val ulong' :  'c C.ulong_obj' -> MLRep.ULong.word
	val float' :  'c C.float_obj' -> MLRep.Float.real
	val double' : 'c C.double_obj' -> MLRep.Double.real

	(* bitfields *)
	val sbf : 'c C.sbf -> MLRep.SInt.int
	val ubf : 'c C.ubf -> MLRep.UInt.word
    end

    (* "store" methods; these require rw objects *)
    structure Set : sig
	(* primitive types *)
	val schar :  C.rw C.schar_obj * MLRep.SChar.int -> unit
	val uchar :  C.rw C.uchar_obj * MLRep.UChar.word -> unit
	val sint :   C.rw C.sint_obj * MLRep.SInt.int -> unit
	val uint :   C.rw C.uint_obj * MLRep.UInt.word -> unit
	val sshort : C.rw C.sshort_obj * MLRep.SShort.int -> unit
	val ushort : C.rw C.ushort_obj * MLRep.UShort.word -> unit
	val slong :  C.rw C.slong_obj * MLRep.SLong.int -> unit
	val ulong :  C.rw C.ulong_obj * MLRep.ULong.word -> unit
	val float :  C.rw C.float_obj * MLRep.Float.real -> unit
	val double : C.rw C.double_obj * MLRep.Double.real -> unit

	(* alt *)
	val schar' :  C.rw C.schar_obj' * MLRep.SChar.int -> unit
	val uchar' :  C.rw C.uchar_obj' * MLRep.UChar.word -> unit
	val sint' :   C.rw C.sint_obj' * MLRep.SInt.int -> unit
	val uint' :   C.rw C.uint_obj' * MLRep.UInt.word -> unit
	val sshort' : C.rw C.sshort_obj' * MLRep.SShort.int -> unit
	val ushort' : C.rw C.ushort_obj' * MLRep.UShort.word -> unit
	val slong' :  C.rw C.slong_obj' * MLRep.SLong.int -> unit
	val ulong' :  C.rw C.ulong_obj' * MLRep.ULong.word -> unit
	val float' :  C.rw C.float_obj' * MLRep.Float.real -> unit
	val double' : C.rw C.double_obj' * MLRep.Double.real -> unit

	(* bitfields *)
	val sbf : C.rw C.sbf * MLRep.SInt.int -> unit
	val ubf : C.rw C.ubf * MLRep.UInt.word -> unit
    end
end

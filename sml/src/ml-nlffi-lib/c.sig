(*
 * Encoding the C type system in SML.
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature C = sig

    (* objects of type 't, constness 'c;
     * The type 'f is an artifact of how function pointers are handled:
     *   - 'f is the 'f in 'f fptr for any type derived (via ptr or arr)
     *     from 'f fptr.
     *   - For all other types, 'f is unit.
     * The 't type variable will be instantiated with the object's "witness"
     * type. The intention is that there be an isomorphism between such
     * witness types and corresponding C types.
     *
     * Witness types are often the same as the (abstract) type of the value
     * stored in the object.  However, this is merely a coincidence.  For
     * example, a constant object holding a pointer to a read-write integer
     * would have type ((sint, unit, rw) ptr, unit, ro) obj and the value
     * itself has type (sint, unit, rw) ptr.
     * However, in the case of the "light" version of this object (see below),
     *  the object type is ((sint, unit, rw) ptr, unit, ro) obj' while fetching
     * from this object gives a value of type (sint, unit, rw) ptr'.
     * (In other words, we use the "heavy" versions of value types as witness
     * types -- even in the "light" case.) *)
    type ('t, 'f, 'c) obj

    (* an alternative "light-weight" version that does not carry RTI at
     * the cost of requiring explicit passing of RTI for certain operations *)
    type ('t, 'f, 'c) obj'

    (* constness property, to be substituted for 'c *)
    type ro
    type rw

    (* things to be substituted for 't *)
    type ('t, 'f, 'c) ptr		(* pointer to ('t, 'f, 'c) obj *)
    type ('t, 'f, 'n) arr		(* 'n-sized array with 't elements *)

    (* light-weight alternative *)
    type ('t, 'f, 'c) ptr'

    (* void* and function pointers *)
    type voidptr			(* C's void* *)
    type 'f fptr			(* a function pointer *)

    (* alt *)
    type 'f fptr'

    (* structures and unions *)
    type 'tag su			(* struct/union named 'tag;
					 * 'tag is drawn from the types
					 * defined in the Tag module *)

    (* primtypes (signed/unsigned char, int, short, long; float, double) *)
    type schar
    type uchar
    type sint
    type uint
    type sshort
    type ushort
    type slong
    type ulong
    type float
    type double

    (* going from abstract to concrete and vice versa *)
    structure Cvt : sig
	(* ML -> C *)
	val c_schar  : MLRep.SChar.int   -> schar
	val c_uchar  : MLRep.UChar.word  -> uchar
	val c_sint   : MLRep.SInt.int    -> sint
	val c_uint   : MLRep.UInt.word   -> uint
	val c_sshort : MLRep.SShort.int  -> sshort
	val c_ushort : MLRep.UShort.word -> ushort
	val c_slong  : MLRep.SLong.int   -> slong
	val c_ulong  : MLRep.ULong.word  -> ulong
	val c_float  : MLRep.Float.real  -> float
	val c_double : MLRep.Double.real -> double

	(* C -> ML *)
	val ml_schar  : schar  -> MLRep.SChar.int
	val ml_uchar  : uchar  -> MLRep.UChar.word
	val ml_sint   : sint   -> MLRep.SInt.int
	val ml_uint   : uint   -> MLRep.UInt.word
	val ml_sshort : sshort -> MLRep.SShort.int
	val ml_ushort : ushort -> MLRep.UShort.word
	val ml_slong  : slong  -> MLRep.SLong.int
	val ml_ulong  : ulong  -> MLRep.ULong.word
	val ml_float  : float  -> MLRep.Float.real
	val ml_double : double -> MLRep.Double.real
    end

    (* type-abbreviations for a bit more convenience. *)
    type 'c schar_obj = (schar, unit, 'c) obj
    type 'c uchar_obj = (uchar, unit, 'c) obj
    type 'c sint_obj = (sint, unit, 'c) obj
    type 'c uint_obj = (uint, unit, 'c) obj
    type 'c sshort_obj = (sshort, unit, 'c) obj
    type 'c ushort_obj = (ushort, unit, 'c) obj
    type 'c slong_obj = (slong, unit, 'c) obj
    type 'c ulong_obj = (ulong, unit, 'c) obj
    type 'c float_obj = (float, unit, 'c) obj
    type 'c double_obj = (double, unit, 'c) obj
    type 'c voidptr_obj = (voidptr, unit, 'c) obj
    type ('f, 'c) fptr_obj = ('f fptr, 'f, 'c) obj
    type ('s, 'c) su_obj = ('s su, unit, 'c) obj

    (* alt *)
    type 'c schar_obj' = (schar, unit, 'c) obj'
    type 'c uchar_obj' = (uchar, unit, 'c) obj'
    type 'c sint_obj' = (sint, unit, 'c) obj'
    type 'c uint_obj' = (uint, unit, 'c) obj'
    type 'c sshort_obj' = (sshort, unit, 'c) obj'
    type 'c ushort_obj' = (ushort, unit, 'c) obj'
    type 'c slong_obj' = (slong, unit, 'c) obj'
    type 'c ulong_obj' = (ulong, unit, 'c) obj'
    type 'c float_obj' = (float, unit, 'c) obj'
    type 'c double_obj' = (double, unit, 'c) obj'
    type 'c voidptr_obj' = (voidptr, unit, 'c) obj'
    type ('f, 'c) fptr_obj' = ('f fptr, 'f, 'c) obj'
    type ('s, 'c) su_obj' = ('s su, unit, 'c) obj'

    (* bitfields aren't "ordinary objects", so they have their own type *)
    type 'c sbf
    type 'c ubf

    (*
     * A family of types and corresponding values representing natural numbers.
     *   (An encoding in SML without using dependent types.)
     *)
    structure Dim : sig

	(* Internally, a value of type ('a, 'z) dim0 is just a number.
	 * The trick here is to give each of these numbers a different unique
	 * type. 'a will be a decimal encoding of the number's value in
	 * "type digits". 'z keeps track of whether the number is zero or not.
	 *)
	type ('a, 'z) dim0

	(* We can always get the internal number back... *)
	val toInt : ('a, 'z) dim0 -> int

	(* These two types act as "flags". They will be substituted for 'z
	 * and remember whether the value is zero or not. *)
	type zero
	type nonzero

	type 'a dim = ('a, nonzero) dim0

	(* These are the "type digits".  Type "dec" acts as a "terminator".
	 * We chose its name so to remind us that the encoding is decimal.
	 * If a nonzero value's decimal representation is "<Dn>...<D0>", then
	 * its type will be "(dec dg<Dn> ... dg<D0>, nonzero) dim0", which
	 * is the same as "dec dg<Dn> ... dg<D0> dim".  The type of the zero
	 * value is "(dec, zero) dim0". *)
	type dec
	type 'a dg0
	type 'a dg1
	type 'a dg2
	type 'a dg3
	type 'a dg4
	type 'a dg5
	type 'a dg6
	type 'a dg7
	type 'a dg8
	type 'a dg9

	(* Here are the corresponding constructors for ('a, 'z) dim0 values.
	 * The type for dg0 ensures that there will be no "leading zeros" in
	 * any encoding.  This guarantees a 1-1 correspondence of constructable
	 * values and their types.
	 * To construct the value corresponding to a nonzero number whose
	 * decimal representation is "<Dn>...<D0>", one must invoke
	 * "dg<D0>' (... (dg<Dn>' dec')...)", i.e., the least significant
	 * digit appears leftmost. *)
	val dec' :            (dec, zero) dim0
	val dg0' : 'a dim        -> 'a dg0 dim
	val dg1' : ('a, 'z) dim0 -> 'a dg1 dim
	val dg2' : ('a, 'z) dim0 -> 'a dg2 dim
	val dg3' : ('a, 'z) dim0 -> 'a dg3 dim
	val dg4' : ('a, 'z) dim0 -> 'a dg4 dim
	val dg5' : ('a, 'z) dim0 -> 'a dg5 dim
	val dg6' : ('a, 'z) dim0 -> 'a dg6 dim
	val dg7' : ('a, 'z) dim0 -> 'a dg7 dim
	val dg8' : ('a, 'z) dim0 -> 'a dg8 dim
	val dg9' : ('a, 'z) dim0 -> 'a dg9 dim

	(* Since having to reverse the sequence of digits seems unnatural,
	 * here is another set of constructors for dim values.  These
	 * constructors use continuation-passing style and themselves
	 * have more complicated types.  But their use is easier:
	 * To construct the value corresponding to a nonzero number whose
	 * decimal representation is "<Dn>...<D0>", one must invoke
	 * "dec dg<Dn> ... dg<D0> dim"; i.e., the least significant
	 * digit appears rightmost -- just like in the usual decimal
	 * notation for numbers that we are all familiar with.
	 * [Moreover, for any 'a dim value we have the neat property that
	 * it can be constructed by taking its type (expressed using "dim")
	 * and interpreting it as an expression.  For example, the dim
	 * value representing 312 is "dec dg3 dg1 dg2 dim" and it can
	 * be constructed by evaluating "dec dg3 dg1 dg2 dim".] *)
	val dec :            ((dec, zero) dim0 -> 'b) -> 'b

	val dg0 : 'a dim        -> ('a dg0 dim -> 'b) -> 'b
	val dg1 : ('a, 'z) dim0 -> ('a dg1 dim -> 'b) -> 'b
	val dg2 : ('a, 'z) dim0 -> ('a dg2 dim -> 'b) -> 'b
	val dg3 : ('a, 'z) dim0 -> ('a dg3 dim -> 'b) -> 'b
	val dg4 : ('a, 'z) dim0 -> ('a dg4 dim -> 'b) -> 'b
	val dg5 : ('a, 'z) dim0 -> ('a dg5 dim -> 'b) -> 'b
	val dg6 : ('a, 'z) dim0 -> ('a dg6 dim -> 'b) -> 'b
	val dg7 : ('a, 'z) dim0 -> ('a dg7 dim -> 'b) -> 'b
	val dg8 : ('a, 'z) dim0 -> ('a dg8 dim -> 'b) -> 'b
	val dg9 : ('a, 'z) dim0 -> ('a dg9 dim -> 'b) -> 'b

	val dim : ('a, 'z) dim0 -> ('a, 'z) dim0
    end

    (* sub-structure for dealing with run-time type info (sizes only) *)
    structure S : sig

	(* Our size info itself is statically typed!
	 * The size info for a value stored in ('t, 'f, 'c) obj has
	 * the following type: *)
	type 't size

	(* get a number out *)
	val toWord : 't size -> word

	(* sizes for simple things *)
	val schar  : schar size
	val uchar  : uchar size
	val sint   : sint size
	val uint   : uint size
	val sshort : sshort size
	val ushort : ushort size
	val slong  : slong size
	val ulong  : ulong size
	val float  : float size
	val double : double size

	val voidptr : voidptr size
	val ptr : ('t, 'f, 'c) ptr size
	val fptr : 'f fptr size
    end

    (* sub-structure for dealing with run-time type info *)
    structure T : sig

	(* Our RTI itself is statically typed!
	 * The RTI for a value stored in ('t, 'f, 'c) obj has
	 * the following type: *)
	type ('t, 'f) typ

	(* again, a lot of abbreviations *)
	type schar_typ = (schar, unit) typ
	type uchar_typ = (uchar, unit) typ
	type sint_typ = (sint, unit) typ
	type uint_typ = (uint, unit) typ
	type sshort_typ = (sshort, unit) typ
	type ushort_typ = (ushort, unit) typ
	type slong_typ = (slong, unit) typ
	type ulong_typ = (ulong, unit) typ
	type float_typ = (float, unit) typ
	type double_typ = (double, unit) typ
	type voidptr_typ = (voidptr, unit) typ
	type 'f fptr_typ = ('f fptr, 'f) typ
	type 's su_typ = ('s su, unit) typ

	(* get the RTI from an actual object *)
	val typeof : ('t, 'f, 'c) obj -> ('t, 'f) typ

	(* constructing new RTI from existing RTI *)
	val pointer : ('t, 'f) typ -> (('t, 'f, rw) ptr, 'f) typ
	val target  : (('t, 'f, 'c) ptr, 'f) typ -> ('t, 'f) typ
	val arr     : ('t, 'f) typ * 'n Dim.dim -> (('t, 'f, 'n) arr, 'f) typ
	val elem    : (('t, 'f, 'n) arr, 'f) typ -> ('t, 'f) typ
	val ro      : (('t, 'f, 'c) ptr, 'f) typ -> (('t, 'f, ro) ptr, 'f) typ

	(* calculating the size of an object given its RTI *)
	val sizeof : ('t, 'f) typ -> 't S.size

	(* dimension of array type *)
	val dim : (('t, 'f, 'n) arr, 'f) typ -> 'n Dim.dim

	(* RTI for simple things *)
	val schar  : schar_typ
	val uchar  : uchar_typ
	val sint   : sint_typ
	val uint   : uint_typ
	val sshort : sshort_typ
	val ushort : ushort_typ
	val slong  : slong_typ
	val ulong  : ulong_typ
	val float  : float_typ
	val double : double_typ

	val voidptr : voidptr_typ
    end

    (* convert from regular (heavy) to alternative (light) versions *)
    structure Light : sig
	val obj : ('t, 'f, 'c) obj -> ('t, 'f, 'c) obj'
	val ptr : ('t, 'f, 'c) ptr -> ('t, 'f, 'c) ptr'
	val fptr : 'f fptr -> 'f fptr'
    end

    (* and vice versa *)
    structure Heavy : sig
	val obj : ('t, 'f) T.typ -> ('t, 'f, 'c) obj' -> ('t, 'f, 'c) obj
	val ptr : ('t, 'f) T.typ -> ('t, 'f, 'c) ptr' -> ('t, 'f, 'c) ptr
	val fptr : 'f T.fptr_typ  -> 'f fptr' -> 'f fptr
    end

    (* calculate size of an object in bytes *)
    val sizeof : ('t, 'f, 'c) obj -> 't S.size

    (* "fetch" methods for various types;
     * fetching does not care about constness *)
    structure Get : sig

	(* primitive types *)
	val schar :  'c schar_obj -> schar
	val uchar :  'c uchar_obj -> uchar
	val sint :   'c sint_obj -> sint
	val uint :   'c uint_obj -> uint
	val sshort : 'c sshort_obj -> sshort
	val ushort : 'c ushort_obj -> ushort
	val slong :  'c slong_obj -> slong
	val ulong :  'c ulong_obj -> ulong
	val float :  'c float_obj -> float
	val double : 'c double_obj -> double

	(* alt *)
	val schar' :  'c schar_obj' -> schar
	val uchar' :  'c uchar_obj' -> uchar
	val sint' :   'c sint_obj' -> sint
	val uint' :   'c uint_obj' -> uint
	val sshort' : 'c sshort_obj' -> sshort
	val ushort' : 'c ushort_obj' -> ushort
	val slong' :  'c slong_obj' -> slong
	val ulong' :  'c ulong_obj' -> ulong
	val float' :  'c float_obj' -> float
	val double' : 'c double_obj' -> double

	(* fetching pointers *)
	val ptr : (('t, 'f, 'pc) ptr, 'f, 'c) obj -> ('t, 'f, 'pc) ptr
	val fptr : ('f, 'c) fptr_obj -> 'f fptr
	val voidptr : 'c voidptr_obj -> voidptr

	(* alt *)
	val ptr' : (('t, 'f, 'pc) ptr, 'f, 'c) obj' -> ('t, 'f, 'pc) ptr'
	val fptr' : ('f, 'c) fptr_obj' -> 'f fptr'
	val voidptr' : 'c voidptr_obj' -> voidptr

	(* bitfields *)
	val sbf : 'c sbf -> sint
	val ubf : 'c ubf -> uint
    end

    (* "store" methods; these require rw objects *)
    structure Set : sig
	(* primitive types *)
	val schar :  rw schar_obj * schar -> unit
	val uchar :  rw uchar_obj * uchar -> unit
	val sint :   rw sint_obj * sint -> unit
	val uint :   rw uint_obj * uint -> unit
	val sshort : rw sshort_obj * sshort -> unit
	val ushort : rw ushort_obj * ushort -> unit
	val slong :  rw slong_obj * slong -> unit
	val ulong :  rw ulong_obj * ulong -> unit
	val float :  rw float_obj * float -> unit
	val double : rw double_obj * double -> unit

	(* alt *)
	val schar' :  rw schar_obj' * schar -> unit
	val uchar' :  rw uchar_obj' * uchar -> unit
	val sint' :   rw sint_obj' * sint -> unit
	val uint' :   rw uint_obj' * uint -> unit
	val sshort' : rw sshort_obj' * sshort -> unit
	val ushort' : rw ushort_obj' * ushort -> unit
	val slong' :  rw slong_obj' * slong -> unit
	val ulong' :  rw ulong_obj' * ulong -> unit
	val float' :  rw float_obj' * float -> unit
	val double' : rw double_obj' * double -> unit

	(* storing pointers *)
	val ptr : (('t, 'f, 'pc) ptr, 'f, rw) obj * ('t, 'f, 'pc) ptr -> unit
	val fptr : ('f, rw) fptr_obj * 'f fptr -> unit
	val voidptr : rw voidptr_obj * voidptr -> unit

	(* alt *)
	val ptr' : (('t, 'f, 'pc) ptr, 'f, rw) obj' * ('t, 'f, 'pc) ptr' -> unit
	val fptr' : ('f, rw) fptr_obj' * 'f fptr' -> unit
	val voidptr' : rw voidptr_obj' * voidptr -> unit

	(* When storing, voidptr is compatible with any ptr type
	 * (just like in C).  This should eliminate most need for RTI in
	 * practice. *)
	val ptr_voidptr : (('t, 'f, 'pc) ptr, 'f, rw) obj * voidptr -> unit

	(* alt *)
	val ptr_voidptr' : (('t, 'f, 'pc) ptr, 'f, rw) obj' * voidptr -> unit

	(* bitfields *)
	val sbf : rw sbf * sint -> unit
	val ubf : rw ubf * uint -> unit
    end

    (* copying the contents of arbitrary objects *)
    val copy : { from: ('t, 'f, 'c) obj, to: ('t, 'f, rw) obj } -> unit

    (* alt *)
    val copy' : 't S.size ->
		{ from: ('t, 'f, 'c) obj', to: ('t, 'f, rw) obj' } -> unit

    (* operations on (mostly) pointers *)
    structure Ptr : sig

	(* going from object to pointer and vice versa *)
	val |&| : ('t, 'f, 'c) obj -> ('t, 'f, 'c) ptr
	val |*| : ('t, 'f, 'c) ptr -> ('t, 'f, 'c) obj

	(* alt *)
	val |&! : ('t, 'f, 'c) obj' -> ('t, 'f, 'c) ptr'
	val |*! : ('t, 'f, 'c) ptr' -> ('t, 'f, 'c) obj'

	(* comparing pointers *)
	val compare : ('t, 'f, 'c) ptr * ('t, 'f, 'c) ptr -> order

	(* alt *)
	val compare' : ('t, 'f, 'c) ptr' * ('t, 'f, 'c) ptr' -> order

	(* going from pointer to void*;  this also accounts for a conceptual
	 * subtyping relation and is safe *)
	val inject : ('t, 'f, 'c) ptr -> voidptr

	(* alt *)
	val inject' : ('t, 'f, 'c) ptr' -> voidptr

	(* the opposite is not safe, but C makes it not only easy but also
	 * almost necessary; we use our RTI interface to specify the pointer
	 * type (not the element type!) *)
	val project : (('t, 'f, 'c) ptr, 'f) T.typ ->
		      voidptr -> ('t, 'f, 'c) ptr

	(* alt *)
	val project' : (('t, 'f, 'c) ptr, 'f) T.typ ->
		       voidptr -> ('t, 'f, 'c) ptr'

	(* NULL as void* *)
	val vNull : voidptr

	(* projecting vNull to given pointer type *)
	val null : (('t, 'f, 'c) ptr, 'f) T.typ -> ('t, 'f, 'c) ptr

	(* the "light" NULL pointer is simply a polymorphic constant *)
	val null' : ('t, 'f, 'c) ptr'

	(* checking for NULL pointer *)
	val vIsNull : voidptr -> bool

	(* combining inject and vIsNull for convenience *)
	val isNull : ('t, 'f, 'c) ptr -> bool

	(* alt *)
	val isNull' : ('t, 'f, 'c) ptr' -> bool

	(* pointer arithmetic *)
	val |+| : ('t, 'f, 'c) ptr * int -> ('t, 'f, 'c) ptr
	val |-| : ('t, 'f, 'c) ptr * ('t, 'f, 'c) ptr -> int

	(* alt; needs explicit size (for element) *)
	val |+! : 't S.size -> ('t, 'f, 'c) ptr' * int -> ('t, 'f, 'c) ptr'
	val |-! : 't S.size -> ('t, 'f, 'c) ptr' * ('t, 'f, 'c) ptr' -> int

	(* subscript through a pointer; this is unchecked *)
	val sub : ('t, 'f, 'c) ptr * int -> ('t, 'f, 'c) obj

	(* alt; needs explicit size (for element) *)
	val sub' : 't S.size -> ('t, 'f, 'c) ptr' * int -> ('t, 'f, 'c) obj'
    end

    (* manipulating object constness
     * rw -> ro:  this direction just accounts for the fact that
     *            rw is conceptually a subtype of ro
     * ro -> rw:  this is not safe, but C makes it so easy that we
     *            might as well directly support it *)
    val ro : ('t, 'f, 'c) obj -> ('t, 'f, ro) obj
    val rw : ('t, 'f, 'c) obj -> ('t, 'f, rw) obj

    (* alt *)
    val ro' : ('t, 'f, 'c) obj' -> ('t, 'f, ro) obj'
    val rw' : ('t, 'f, 'c) obj' -> ('t, 'f, rw) obj'

    (* operations on (mostly) arrays *)
    structure Arr : sig
	
	(* array subscript;
	 * since we have RTI, we can actually make this safe:  we raise
	 * General.Subscript for out-of-bounds access;
	 * for unchecked access, go through arr_decay and ptr_sub *)
	val sub : (('t, 'f, 'n) arr, 'f, 'c) obj * int -> ('t, 'f, 'c) obj

	(* alt; needs explicit type (for array) *)
	val sub' : (('t, 'f, 'n) arr, 'f) T.typ ->
		   (('t, 'f, 'n) arr, 'f, 'c) obj' * int -> ('t, 'f, 'c) obj'

	(* let an array object decay, yielding pointer to first element *)
	val decay : (('t, 'f, 'n) arr, 'f, 'c) obj -> ('t, 'f, 'c) ptr

	(* alt *)
	val decay' : (('t, 'f, 'n) arr, 'f, 'c) obj' -> ('t, 'f, 'c) ptr'

	(* reconstruct an array object from the pointer to its first element *)
	val reconstruct :
	    ('t, 'f, 'c) ptr * 'n Dim.dim -> (('t, 'f, 'n) arr, 'f, 'c) obj

	(* alt *)
	val reconstruct' :
	    ('t, 'f, 'c) ptr' * 'n Dim.dim -> (('t, 'f, 'n) arr, 'f, 'c) obj'

	(* dimension of array object *)
	val dim : (('t, 'f, 'n) arr, 'f, 'c) obj -> 'n Dim.dim
    end

    (* allocating new objects *)
    val new : ('t, 'f) T.typ -> ('t, 'f, rw) obj option

    (* alt *)
    val new' : ('t, 'f) T.typ -> ('t, 'f, rw) obj' option

    (* allocating something that does not involve function pointers... *)
    val new'' : 't S.size -> ('t, unit, rw) obj' option

    (* freeing objects that were allocated earlier *)
    val discard : ('t, 'f, 'c) obj -> unit

    (* alt *)
    val discard' : ('t, 'f, 'c) obj' -> unit

    (* allocating a dynamically-sized array *)
    val alloc : ('t, 'f) T.typ -> word -> ('t, 'f, rw) ptr option

    (* alt *)
    val alloc' : ('t, 'f) T.typ -> word -> ('t, 'f, rw) ptr' option

    (* alt, without function pointers *)
    val alloc'' : 't S.size -> word -> ('t, unit, rw) ptr' option

    (* freeing through pointers *)
    val free : ('t, 'f, 'c) ptr -> unit

    (* alt *)
    val free' : ('t, 'f, 'c) ptr' -> unit

    (* perform function call through function-pointer *)
    val call : ('a -> 'b) fptr * 'a -> 'b

    (* alt; needs explicit type for the function pointer *)
    val call' : (('a -> 'b) fptr, 'a -> 'b) T.typ ->
		('a -> 'b) fptr' * 'a -> 'b
end

(*
 * Encoding the C type system in SML.
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature C = sig

    exception OutOfMemory

    (* objects of type 't, constness 'c;
     * The 't type variable will be instantiated with the object's "witness"
     * type. The intention is that there be an isomorphism between such
     * witness types and corresponding C types.
     *
     * Witness types are often the same as the (abstract) type of the value
     * stored in the object.  However, this is merely a coincidence.  For
     * example, a constant object holding a pointer to a read-write integer
     * would have type ((sint, rw) ptr, ro) obj and the value itself has
     * type (sint, rw) ptr.
     * However, in the case of the "light" version of this object (see below),
     * the object type is ((sint, rw) ptr, ro) obj' while fetching
     * from this object gives a value of type (sint, rw) ptr'.
     * (In other words, we use the "heavy" versions of value types as witness
     * types -- even in the "light" case.) *)
    type ('t, 'c) obj

    (* an alternative "light-weight" version that does not carry RTTI at
     * the cost of requiring explicit passing of RTTI for certain operations *)
    eqtype ('t, 'c) obj'

    (* constness property, to be substituted for 'c *)
    type ro and rw

    (* things to be substituted for 't *)
    type ('t, 'c) ptr			(* pointer to ('t, 'c) obj *)
    type ('t, 'n) arr			(* 'n-sized array with 't elements *)

    (* light-weight alternative *)
    eqtype ('t, 'c) ptr'

    (* void* and function pointers *)
    eqtype voidptr			(* C's void* *)
    type 'f fptr			(* a function pointer *)

    (* alt *)
    eqtype 'f fptr'

    (* structures and unions *)
    type 'tag su			(* struct/union named 'tag;
					 * 'tag is drawn from the types
					 * defined in the Tag module *)

    (* primtypes (signed/unsigned char, int, short, long; float, double) *)
    eqtype schar  and uchar
    eqtype sint   and uint
    eqtype sshort and ushort
    eqtype slong  and ulong
    type float    and double

    (* going from abstract to concrete and vice versa *)
    structure Cvt : sig
	(* ML -> C *)
	val c_schar  : MLRep.Signed.int   -> schar
	val c_uchar  : MLRep.Unsigned.word  -> uchar
	val c_sint   : MLRep.Signed.int    -> sint
	val c_uint   : MLRep.Unsigned.word   -> uint
	val c_sshort : MLRep.Signed.int  -> sshort
	val c_ushort : MLRep.Unsigned.word -> ushort
	val c_slong  : MLRep.Signed.int   -> slong
	val c_ulong  : MLRep.Unsigned.word  -> ulong
	val c_float  : MLRep.Real.real  -> float
	val c_double : MLRep.Real.real -> double

	(* C -> ML *)
	val ml_schar  : schar  -> MLRep.Signed.int
	val ml_uchar  : uchar  -> MLRep.Unsigned.word
	val ml_sint   : sint   -> MLRep.Signed.int
	val ml_uint   : uint   -> MLRep.Unsigned.word
	val ml_sshort : sshort -> MLRep.Signed.int
	val ml_ushort : ushort -> MLRep.Unsigned.word
	val ml_slong  : slong  -> MLRep.Signed.int
	val ml_ulong  : ulong  -> MLRep.Unsigned.word
	val ml_float  : float  -> MLRep.Real.real
	val ml_double : double -> MLRep.Real.real
    end

    (* type-abbreviations for a bit more convenience. *)
    type 'c schar_obj = (schar, 'c) obj
    type 'c uchar_obj = (uchar, 'c) obj
    type 'c sint_obj = (sint, 'c) obj
    type 'c uint_obj = (uint, 'c) obj
    type 'c sshort_obj = (sshort, 'c) obj
    type 'c ushort_obj = (ushort, 'c) obj
    type 'c slong_obj = (slong, 'c) obj
    type 'c ulong_obj = (ulong, 'c) obj
    type 'c float_obj = (float, 'c) obj
    type 'c double_obj = (double, 'c) obj
    type 'c voidptr_obj = (voidptr, 'c) obj
    type ('f, 'c) fptr_obj = ('f fptr, 'c) obj
    type ('s, 'c) su_obj = ('s su, 'c) obj

    (* alt *)
    type 'c schar_obj' = (schar, 'c) obj'
    type 'c uchar_obj' = (uchar, 'c) obj'
    type 'c sint_obj' = (sint, 'c) obj'
    type 'c uint_obj' = (uint, 'c) obj'
    type 'c sshort_obj' = (sshort, 'c) obj'
    type 'c ushort_obj' = (ushort, 'c) obj'
    type 'c slong_obj' = (slong, 'c) obj'
    type 'c ulong_obj' = (ulong, 'c) obj'
    type 'c float_obj' = (float, 'c) obj'
    type 'c double_obj' = (double, 'c) obj'
    type 'c voidptr_obj' = (voidptr, 'c) obj'
    type ('f, 'c) fptr_obj' = ('f fptr, 'c) obj'
    type ('s, 'c) su_obj' = ('s su, 'c) obj'

    (* bitfields aren't "ordinary objects", so they have their own type *)
    eqtype 'c sbf and 'c ubf

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
	type 'a dg0 and 'a dg1 and 'a dg2 and 'a dg3 and 'a dg4
	type 'a dg5 and 'a dg6 and 'a dg7 and 'a dg8 and 'a dg9

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
	 * The size info for a value stored in ('t, 'c) obj has
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
	val ptr : ('t, 'c) ptr size
	val fptr : 'f fptr size
    end

    (* sub-structure for dealing with run-time type info *)
    structure T : sig

	(* Our RTTI itself is statically typed!
	 * The RTTI for a value stored in ('t, 'c) obj has
	 * the following type: *)
	type 't typ

	(* get the RTTI from an actual object *)
	val typeof : ('t, 'c) obj -> 't typ

	(* constructing new RTTI from existing RTTI *)
	val pointer : 't typ -> ('t, rw) ptr typ
	val target  : ('t, 'c) ptr typ -> 't typ
	val arr     : 't typ * 'n Dim.dim -> ('t, 'n) arr typ
	val elem    : ('t, 'n) arr typ -> 't typ
	val ro      : ('t, 'c) ptr typ -> ('t, ro) ptr typ

	(* calculating the size of an object given its RTTI *)
	val sizeof : 't typ -> 't S.size

	(* dimension of array type *)
	val dim : ('t, 'n) arr typ -> 'n Dim.dim

	(* RTTI for simple things *)
	val schar  : schar typ
	val uchar  : uchar typ
	val sint   : sint typ
	val uint   : uint typ
	val sshort : sshort typ
	val ushort : ushort typ
	val slong  : slong typ
	val ulong  : ulong typ
	val float  : float typ
	val double : double typ

	val voidptr : voidptr typ
    end

    (* convert from regular (heavy) to alternative (light) versions *)
    structure Light : sig
	val obj : ('t, 'c) obj -> ('t, 'c) obj'
	val ptr : ('t, 'c) ptr -> ('t, 'c) ptr'
	val fptr : 'f fptr -> 'f fptr'
    end

    (* and vice versa *)
    structure Heavy : sig
	val obj : 't T.typ -> ('t, 'c) obj' -> ('t, 'c) obj
	val ptr : 't T.typ -> ('t, 'c) ptr' -> ('t, 'c) ptr
	val fptr : 'f fptr T.typ  -> 'f fptr' -> 'f fptr
    end

    (* calculate size of an object *)
    val sizeof : ('t, 'c) obj -> 't S.size

    (* "fetch" methods for various types;
     * fetching does not care about constness *)
    structure Get : sig

	(* primitive types; the results are concrete here *)
	val schar :  'c schar_obj -> MLRep.Signed.int
	val uchar :  'c uchar_obj -> MLRep.Unsigned.word
	val sint :   'c sint_obj -> MLRep.Signed.int
	val uint :   'c uint_obj -> MLRep.Unsigned.word
	val sshort : 'c sshort_obj -> MLRep.Signed.int
	val ushort : 'c ushort_obj -> MLRep.Unsigned.word
	val slong :  'c slong_obj -> MLRep.Signed.int
	val ulong :  'c ulong_obj -> MLRep.Unsigned.word
	val float :  'c float_obj -> MLRep.Real.real
	val double : 'c double_obj -> MLRep.Real.real

	(* alt *)
	val schar' :  'c schar_obj' -> MLRep.Signed.int
	val uchar' :  'c uchar_obj' -> MLRep.Unsigned.word
	val sint' :   'c sint_obj' -> MLRep.Signed.int
	val uint' :   'c uint_obj' -> MLRep.Unsigned.word
	val sshort' : 'c sshort_obj' -> MLRep.Signed.int
	val ushort' : 'c ushort_obj' -> MLRep.Unsigned.word
	val slong' :  'c slong_obj' -> MLRep.Signed.int
	val ulong' :  'c ulong_obj' -> MLRep.Unsigned.word
	val float' :  'c float_obj' -> MLRep.Real.real
	val double' : 'c double_obj' -> MLRep.Real.real

	(* fetching pointers; results have to be abstract *)
	val ptr : (('t, 'pc) ptr, 'c) obj -> ('t, 'pc) ptr
	val fptr : ('f, 'c) fptr_obj -> 'f fptr
	val voidptr : 'c voidptr_obj -> voidptr

	(* alt *)
	val ptr' : (('t, 'pc) ptr, 'c) obj' -> ('t, 'pc) ptr'
	val fptr' : ('f, 'c) fptr_obj' -> 'f fptr'
	val voidptr' : 'c voidptr_obj' -> voidptr

	(* bitfields; concrete again *)
	val sbf : 'c sbf -> MLRep.Signed.int
	val ubf : 'c ubf -> MLRep.Unsigned.word
    end

    (* "store" methods; these require rw objects *)
    structure Set : sig
	(* primitive types; use concrete values *)
	val schar :  rw schar_obj * MLRep.Signed.int -> unit
	val uchar :  rw uchar_obj * MLRep.Unsigned.word -> unit
	val sint :   rw sint_obj * MLRep.Signed.int -> unit
	val uint :   rw uint_obj * MLRep.Unsigned.word -> unit
	val sshort : rw sshort_obj * MLRep.Signed.int -> unit
	val ushort : rw ushort_obj * MLRep.Unsigned.word -> unit
	val slong :  rw slong_obj * MLRep.Signed.int -> unit
	val ulong :  rw ulong_obj * MLRep.Unsigned.word -> unit
	val float :  rw float_obj * MLRep.Real.real -> unit
	val double : rw double_obj * MLRep.Real.real -> unit

	(* alt *)
	val schar' :  rw schar_obj' * MLRep.Signed.int -> unit
	val uchar' :  rw uchar_obj' * MLRep.Unsigned.word -> unit
	val sint' :   rw sint_obj' * MLRep.Signed.int -> unit
	val uint' :   rw uint_obj' * MLRep.Unsigned.word -> unit
	val sshort' : rw sshort_obj' * MLRep.Signed.int -> unit
	val ushort' : rw ushort_obj' * MLRep.Unsigned.word -> unit
	val slong' :  rw slong_obj' * MLRep.Signed.int -> unit
	val ulong' :  rw ulong_obj' * MLRep.Unsigned.word -> unit
	val float' :  rw float_obj' * MLRep.Real.real -> unit
	val double' : rw double_obj' * MLRep.Real.real -> unit

	(* storing pointers; abstract *)
	val ptr : (('t, 'pc) ptr, rw) obj * ('t, 'pc) ptr -> unit
	val fptr : ('f, rw) fptr_obj * 'f fptr -> unit
	val voidptr : rw voidptr_obj * voidptr -> unit

	(* alt *)
	val ptr' : (('t, 'pc) ptr, rw) obj' * ('t, 'pc) ptr' -> unit
	val fptr' : ('f, rw) fptr_obj' * 'f fptr' -> unit
	val voidptr' : rw voidptr_obj' * voidptr -> unit

	(* When storing, voidptr is compatible with any ptr type
	 * (just like in C).  This should eliminate most need for RTTI in
	 * practice. *)
	val ptr_voidptr : (('t, 'pc) ptr, rw) obj * voidptr -> unit

	(* alt *)
	val ptr_voidptr' : (('t, 'pc) ptr, rw) obj' * voidptr -> unit

	(* bitfields; concrete *)
	val sbf : rw sbf * MLRep.Signed.int -> unit
	val ubf : rw ubf * MLRep.Unsigned.word -> unit
    end

    (* copying the contents of arbitrary objects *)
    val copy : { from: ('t, 'c) obj, to: ('t, rw) obj } -> unit

    (* alt *)
    val copy' : 't S.size -> { from: ('t, 'c) obj', to: ('t, rw) obj' } -> unit

    (* manipulating object constness
     * rw -> ro:  this direction just accounts for the fact that
     *            rw is conceptually a subtype of ro
     * ro -> rw:  this is not safe, but C makes it so easy that we
     *            might as well directly support it;
     * Concretely, we make both operations polymorphic in the argument
     * constness.  Moreover, the second (unsafe) direction is also
     * polymorphic in the result.  This can be used to effectively
     * implement a conversion to "whatever the context wants":
     *)
    val ro : ('t, 'c) obj  -> ('t, ro) obj
    val rw : ('t, 'sc) obj -> ('t, 'tc) obj

    (* alt *)
    val ro' : ('t, 'c) obj'  -> ('t, ro) obj'
    val rw' : ('t, 'sc) obj' -> ('t, 'tc) obj'

    (* operations on (mostly) pointers *)
    structure Ptr : sig

	(* going from object to pointer and vice versa *)
	val |&| : ('t, 'c) obj -> ('t, 'c) ptr
	val |*| : ('t, 'c) ptr -> ('t, 'c) obj

	(* alt *)
	val |&! : ('t, 'c) obj' -> ('t, 'c) ptr'
	val |*! : ('t, 'c) ptr' -> ('t, 'c) obj'

	(* comparing pointers *)
	val compare : ('t, 'c) ptr * ('t, 'c) ptr -> order

	(* alt *)
	val compare' : ('t, 'c) ptr' * ('t, 'c) ptr' -> order

	(* going from pointer to void*;  this also accounts for a conceptual
	 * subtyping relation and is safe *)
	val inject : ('t, 'c) ptr -> voidptr

	(* alt *)
	val inject' : ('t, 'c) ptr' -> voidptr

	(* the opposite is not safe, but C makes it not only easy but also
	 * almost necessary; we use our RTTI interface to specify the pointer
	 * type (not the element type!) *)
	val cast : ('t, 'c) ptr T.typ -> voidptr -> ('t, 'c) ptr

	(* alt *)
	val cast' : ('t, 'c) ptr T.typ -> voidptr -> ('t, 'c) ptr'

	(* NULL as void* *)
	val vNull : voidptr

	(* projecting vNull to given pointer type *)
	val null : ('t, 'c) ptr T.typ -> ('t, 'c) ptr

	(* the "light" NULL pointer is simply a polymorphic constant *)
	val null' : ('t, 'c) ptr'

	(* fptr version of NULL *)
	val fnull : 'f fptr T.typ -> 'f fptr

	(* again, "light" version is simply a polymorphic constant *)
	val fnull' : 'f fptr'

	(* checking for NULL pointer *)
	val vIsNull : voidptr -> bool

	(* combining inject and vIsNull for convenience *)
	val isNull : ('t, 'c) ptr -> bool

	(* alt *)
	val isNull' : ('t, 'c) ptr' -> bool

	(* checking a function pointer for NULL *)
	val isFNull : 'f fptr -> bool

	(* alt *)
	val isFNull' : 'f fptr' -> bool

	(* pointer arithmetic *)
	val |+| : ('t, 'c) ptr * int -> ('t, 'c) ptr
	val |-| : ('t, 'c) ptr * ('t, 'c) ptr -> int

	(* alt; needs explicit size (for element) *)
	val |+! : 't S.size -> ('t, 'c) ptr' * int -> ('t, 'c) ptr'
	val |-! : 't S.size -> ('t, 'c) ptr' * ('t, 'c) ptr' -> int

	(* subscript through a pointer; this is unchecked *)
	val sub : ('t, 'c) ptr * int -> ('t, 'c) obj

	(* alt; needs explicit size (for element) *)
	val sub' : 't S.size -> ('t, 'c) ptr' * int -> ('t, 'c) obj'

	(* constness manipulation for pointers *)
	val ro : ('t, 'c) ptr    -> ('t, ro) ptr
	val rw : ('t, 'sc) ptr   -> ('t, 'tc) ptr
	val ro' : ('t, 'c) ptr'  -> ('t, ro) ptr'
	val rw' : ('t, 'sc) ptr' -> ('t, 'tc) ptr'
    end

    (* operations on (mostly) arrays *)
    structure Arr : sig
	
	(* array subscript;
	 * since we have RTTI, we can actually make this safe:  we raise
	 * General.Subscript for out-of-bounds access;
	 * for unchecked access, go through arr_decay and ptr_sub *)
	val sub : (('t, 'n) arr, 'c) obj * int -> ('t, 'c) obj

	(* alt; needs explicit type (for array) *)
	val sub' : ('t, 'n) arr T.typ ->
		   (('t, 'n) arr, 'c) obj' * int -> ('t, 'c) obj'

	(* let an array object decay, yielding pointer to first element *)
	val decay : (('t, 'n) arr, 'c) obj -> ('t, 'c) ptr

	(* alt *)
	val decay' : (('t, 'n) arr, 'c) obj' -> ('t, 'c) ptr'

	(* reconstruct an array object from the pointer to its first element *)
	val reconstruct : ('t, 'c) ptr * 'n Dim.dim -> (('t, 'n) arr, 'c) obj

	(* alt *)
	val reconstruct': ('t, 'c) ptr' * 'n Dim.dim -> (('t, 'n) arr, 'c) obj'

	(* dimension of array object *)
	val dim : (('t, 'n) arr, 'c) obj -> 'n Dim.dim
    end

    (* allocating new objects *)
    val new : 't T.typ -> ('t, 'c) obj

    (* alt *)
    val new' : 't S.size -> ('t, 'c) obj'

    (* freeing objects that were allocated earlier *)
    val discard : ('t, 'c) obj -> unit

    (* alt *)
    val discard' : ('t, 'c) obj' -> unit

    (* allocating a dynamically-sized array *)
    val alloc : 't T.typ -> word -> ('t, 'c) ptr

    (* alt *)
    val alloc' : 't S.size -> word -> ('t, 'c) ptr'

    (* freeing through pointers *)
    val free : ('t, 'c) ptr -> unit

    (* alt *)
    val free' : ('t, 'c) ptr' -> unit

    (* perform function call through function-pointer *)
    val call : ('a -> 'b) fptr * 'a -> 'b

    (* alt; needs explicit type for the function pointer *)
    val call' : ('a -> 'b) fptr T.typ -> ('a -> 'b) fptr' * 'a -> 'b

    (* completely unsafe stuff that every C programmer just *loves* to do *)
    structure U : sig
	val fcast : 'a fptr' -> 'b fptr'
	val p2i : voidptr -> ulong
	val i2p : ulong -> voidptr
    end
end

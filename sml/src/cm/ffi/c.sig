(*
 * Encoding the C type system in SML.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature C = sig

    (* objects of type 't, constness 'c;
     * The type 'f is an artifact of how function pointers are handled:
     *   - 'f is the 'f in 'f fptr for any type derived (via ptr or arr)
     *     from 'f fptr.
     *   - For all other types, 'f is unit. *)
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

    (* corresponding "revealed" types. *)
    type ml_schar  = CMemory.schar
    type ml_uchar  = CMemory.uchar
    type ml_sint   = CMemory.sint
    type ml_uint   = CMemory.uint
    type ml_sshort = CMemory.sshort
    type ml_ushort = CMemory.ushort
    type ml_slong  = CMemory.slong
    type ml_ulong  = CMemory.ulong
    type ml_float  = CMemory.float
    type ml_double = CMemory.double

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

    (* struct field accessors (rw/ro);
     * struct object with tag 's --> object of type 'm *)
    type ('s, 'm, 'f) rw_acc
    type ('s, 'm, 'f) ro_acc

    (* light-weight alternatives *)
    type ('s, 'm, 'f) rw_acc'
    type ('s, 'm, 'f) ro_acc'

    (* bitfields aren't "ordinary objects", so they have their own type *)
    type 'c bitfield

    (* bitfield accessors *)
    type 's bf_rw_acc
    type 's bf_ro_acc

    (* array dimensions; must be non-zero *)
    type 'n dim = ('n, Dim.nonzero) Dim.dim

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
	val arr     : ('t, 'f) typ * 'n dim -> (('t, 'f, 'n) arr, 'f) typ
	val elem    : (('t, 'f, 'n) arr, 'f) typ -> ('t, 'f) typ
	val ro      : (('t, 'f, 'c) ptr, 'f) typ -> (('t, 'f, ro) ptr, 'f) typ

	(* calculating the size of an object given its RTI *)
	val sizeof : ('t, 'f) typ -> word

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

    (* convert from regular (heavy) to alternative (light) versions
     * and vice versa *)
    val light_obj :    ('t, 'f, 'c) obj -> ('t, 'f, 'c) obj'
    val heavy_obj :    ('t, 'f) T.typ ->
		       ('t, 'f, 'c) obj' -> ('t, 'f, 'c) obj
    val light_ptr :    ('t, 'f, 'c) ptr -> ('t, 'f, 'c) ptr'
    val heavy_ptr :    ('t, 'f) T.typ ->
		       ('t, 'f, 'c) ptr' -> ('t, 'f, 'c) ptr
    val light_rw_acc : ('s, 'm, 'f) rw_acc -> ('s, 'm, 'f) rw_acc'
    val heavy_rw_acc : ('m, 'f) T.typ ->
		       ('s, 'm, 'f) rw_acc' -> ('s, 'm, 'f) rw_acc
    val light_ro_acc : ('s, 'm, 'f) ro_acc -> ('s, 'm, 'f) ro_acc'
    val heavy_ro_acc : ('m, 'f) T.typ ->
		       ('s, 'm, 'f) ro_acc' -> ('s, 'm, 'f) ro_acc
    val light_fptr : 'f fptr -> 'f fptr'
    val heavy_fptr : 'f T.fptr_typ  -> 'f fptr' -> 'f fptr

    (* calculate size of an object in bytes *)
    val sizeof : ('t, 'f, 'c) obj -> word

    (* "fetch" methods for various primitive types;
     * fetching does not care about constness *)
    val get_schar :  'c schar_obj -> ml_schar
    val get_uchar :  'c uchar_obj -> ml_uchar
    val get_sint :   'c sint_obj -> ml_sint
    val get_uint :   'c uint_obj -> ml_uint
    val get_sshort : 'c sshort_obj -> ml_sshort
    val get_ushort : 'c ushort_obj -> ml_ushort
    val get_slong :  'c slong_obj -> ml_slong
    val get_ulong :  'c ulong_obj -> ml_ulong
    val get_float :  'c float_obj -> ml_float
    val get_double : 'c double_obj -> ml_double

    (* alt *)
    val get_schar' :  'c schar_obj' -> ml_schar
    val get_uchar' :  'c uchar_obj' -> ml_uchar
    val get_sint' :   'c sint_obj' -> ml_sint
    val get_uint' :   'c uint_obj' -> ml_uint
    val get_sshort' : 'c sshort_obj' -> ml_sshort
    val get_ushort' : 'c ushort_obj' -> ml_ushort
    val get_slong' :  'c slong_obj' -> ml_slong
    val get_ulong' :  'c ulong_obj' -> ml_ulong
    val get_float' :  'c float_obj' -> ml_float
    val get_double' : 'c double_obj' -> ml_double

    (* fetching pointers *)
    val get_ptr :     (('t, 'f, 'pc) ptr, 'f, 'c) obj -> ('t, 'f, 'pc) ptr
    val get_fptr :    ('f, 'c) fptr_obj -> 'f fptr
    val get_voidptr : 'c voidptr_obj -> voidptr

    (* alt *)
    val get_ptr' :     (('t, 'f, 'pc) ptr, 'f, 'c) obj' -> ('t, 'f, 'pc) ptr'
    val get_fptr' :    ('f, 'c) fptr_obj' -> 'f fptr'
    val get_voidptr' : 'c voidptr_obj' -> voidptr

    (* "store" methods; these require rw objects *)
    val set_schar :  rw schar_obj * ml_schar -> unit
    val set_uchar :  rw uchar_obj * ml_uchar -> unit
    val set_sint :   rw sint_obj * ml_sint -> unit
    val set_uint :   rw uint_obj * ml_uint -> unit
    val set_sshort : rw sshort_obj * ml_sshort -> unit
    val set_ushort : rw ushort_obj * ml_ushort -> unit
    val set_slong :  rw slong_obj * ml_slong -> unit
    val set_ulong :  rw ulong_obj * ml_ulong -> unit
    val set_float :  rw float_obj * ml_float -> unit
    val set_double : rw double_obj * ml_double -> unit

    (* alt *)
    val set_schar' :  rw schar_obj' * ml_schar -> unit
    val set_uchar' :  rw uchar_obj' * ml_uchar -> unit
    val set_sint' :   rw sint_obj' * ml_sint -> unit
    val set_uint' :   rw uint_obj' * ml_uint -> unit
    val set_sshort' : rw sshort_obj' * ml_sshort -> unit
    val set_ushort' : rw ushort_obj' * ml_ushort -> unit
    val set_slong' :  rw slong_obj' * ml_slong -> unit
    val set_ulong' :  rw ulong_obj' * ml_ulong -> unit
    val set_float' :  rw float_obj' * ml_float -> unit
    val set_double' : rw double_obj' * ml_double -> unit

    (* storing pointers *)
    val set_ptr : (('t, 'f, 'pc) ptr, 'f, rw) obj * ('t, 'f, 'pc) ptr -> unit
    val set_fptr : ('f, rw) fptr_obj * 'f fptr -> unit
    val set_voidptr : rw voidptr_obj * voidptr -> unit

    (* alt *)
    val set_ptr' : (('t, 'f, 'pc) ptr, 'f, rw) obj' * ('t, 'f, 'pc) ptr'
		   -> unit
    val set_fptr' : ('f, rw) fptr_obj' * 'f fptr' -> unit
    val set_voidptr' : rw voidptr_obj' * voidptr -> unit

    (* When storing, voidptr is compatible with any ptr type
     * (just like in C).  This should eliminate most need for RTI in
     * practice. *)
    val set_ptr_voidptr : (('t, 'f, 'pc) ptr, 'f, rw) obj * voidptr -> unit

    (* alt *)
    val set_ptr_voidptr' : (('t, 'f, 'pc) ptr, 'f, rw) obj' * voidptr -> unit

    (* copying the contents of arbitrary objects *)
    val copy : { from: ('t, 'f, 'c) obj, to: ('t, 'f, rw) obj } -> unit

    (* alt *)
    val copy' : ('t, 'f) T.typ ->
		{ from: ('t, 'f, 'c) obj', to: ('t, 'f, rw) obj' } -> unit

    (* going from object to pointer and vice versa *)
    val |&| : ('t, 'f, 'c) obj -> ('t, 'f, 'c) ptr
    val |*| : ('t, 'f, 'c) ptr -> ('t, 'f, 'c) obj

    (* alt *)
    val |&&| : ('t, 'f, 'c) obj' -> ('t, 'f, 'c) ptr'
    val |**| : ('t, 'f, 'c) ptr' -> ('t, 'f, 'c) obj'

    (* comparing pointers *)
    val ptr_compare : ('t, 'f, 'c) ptr * ('t, 'f, 'c) ptr -> order

    (* alt *)
    val ptr_compare' : ('t, 'f, 'c) ptr' * ('t, 'f, 'c) ptr' -> order

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

    (* going from pointer to void*;  this also accounts for a conceptual
     * subtyping relation and is safe *)
    val ptr_inject : ('t, 'f, 'c) ptr -> voidptr

    (* alt *)
    val ptr_inject' : ('t, 'f, 'c) ptr' -> voidptr

    (* the opposite is not safe, but C makes it not only easy but also almost
     * necessary; we use our RTI interface to specify the pointer type (not
     * the element type!) *)
    val ptr_project : (('t, 'f, 'c) ptr, 'f) T.typ ->
		      voidptr -> ('t, 'f, 'c) ptr

    (* alt *)
    val ptr_project' : (('t, 'f, 'c) ptr, 'f) T.typ ->
		       voidptr -> ('t, 'f, 'c) ptr'

    (* array subscript;
     * since we have RTI, we can actually make this safe:  we raise
     * General.Subscript for out-of-bounds access;
     * for unchecked access, go through arr_decay and ptr_sub *)
    val arr_sub : (('t, 'f, 'n) arr, 'f, 'c) obj * int -> ('t, 'f, 'c) obj

    (* alt; needs explicit type (for array) *)
    val arr_sub' : (('t, 'f, 'n) arr, 'f) T.typ ->
		   (('t, 'f, 'n) arr, 'f, 'c) obj' * int -> ('t, 'f, 'c) obj'

    (* let an array object decay, yielding a pointer to its first element *)
    val arr_decay : (('t, 'f, 'n) arr, 'f, 'c) obj -> ('t, 'f, 'c) ptr

    (* alt *)
    val arr_decay' : (('t, 'f, 'n) arr, 'f, 'c) obj' -> ('t, 'f, 'c) ptr'

    (* reconstruct an array object from the pointer to its first element *)
    val arr_reconstruct :
	('t, 'f, 'c) ptr * 'n dim -> (('t, 'f, 'n) arr, 'f, 'c) obj

    (* alt *)
    val arr_reconstruct' :
	('t, 'f, 'c) ptr' * 'n dim -> (('t, 'f, 'n) arr, 'f, 'c) obj'

    (* NULL is a void* here *)
    val null : voidptr

    (* checking for NULL pointer *)
    val voidptr_null : voidptr -> bool

    (* combining ptr_inject and voidptr_null for convenience *)
    val ptr_null : ('t, 'f, 'c) ptr -> bool

    (* alt *)
    val ptr_null' : ('t, 'f, 'c) ptr' -> bool

    (* pointer arithmetic *)
    val |+| : ('t, 'f, 'c) ptr * int -> ('t, 'f, 'c) ptr
    val |-| : ('t, 'f, 'c) ptr * ('t, 'f, 'c) ptr -> int

    (* alt; needs explicit type (for element) *)
    val |++| : ('t, 'f) T.typ -> ('t, 'f, 'c) ptr' * int -> ('t, 'f, 'c) ptr'
    val |--| : ('t, 'f) T.typ -> ('t, 'f, 'c) ptr' * ('t, 'f, 'c) ptr' -> int

    (* subscript through a pointer; this is an unchecked subscript operation *)
    val ptr_sub : ('t, 'f, 'c) ptr * int -> ('t, 'f, 'c) obj

    (* alt; needs explicit type (for element) *)
    val ptr_sub' : ('t, 'f) T.typ ->
		   ('t, 'f, 'c) ptr' * int -> ('t, 'f, 'c) obj'

    (* representing the dot "." of C's struct/union field access;
     * We use four different dots:
     *   |$| : access to ordinary field that was not declared const
     *   |$! : access to ordinary field that was declared const
     *   |#| : access to bitfield that was not declared const
     *   |#! : access to bitfield that was declared const
     * A | | method propagates constness from the struct/union object to
     * the field object; a | ! method always yields ro objects. *)
    val |$| : ('s, 'c) su_obj * ('s, 'm, 'f) rw_acc -> ('m, 'f, 'c) obj
    val |$! : ('s, 'c) su_obj * ('s, 'm, 'f) ro_acc -> ('m, 'f, ro) obj
    val |#| : ('s, 'c) su_obj * 's bf_rw_acc -> 'c bitfield
    val |#! : ('s, 'c) su_obj * 's bf_ro_acc -> ro bitfield

    (* alt *)
    val |$$| : ('s, 'c) su_obj' * ('s, 'm, 'f) rw_acc' -> ('m, 'f, 'c) obj'
    val |$$! : ('s, 'c) su_obj' * ('s, 'm, 'f) ro_acc' -> ('m, 'f, ro) obj'
    val |##| : ('s, 'c) su_obj' * 's bf_rw_acc -> 'c bitfield
    val |##! : ('s, 'c) su_obj' * 's bf_ro_acc -> ro bitfield

    (* "fetch" and "store" for bitfields *)
    val get_bf : 'c bitfield -> ml_uint
    val set_bf : rw bitfield * ml_uint -> unit

    (* allocating new objects *)
    val new : ('t, 'f) T.typ -> ('t, 'f, rw) obj option

    (* alt *)
    val new' : ('t, 'f) T.typ -> ('t, 'f, rw) obj' option

    (* freeing objects that were allocated earlier *)
    val discard : ('t, 'f, 'c) obj -> unit

    (* alt *)
    val discard' : ('t, 'f, 'c) obj' -> unit

    (* perform function call through function-pointer *)
    val call : ('a -> 'b) fptr * 'a -> 'b

    (* alt; needs explicit type for the function pointer *)
    val call' : (('a -> 'b) fptr, 'a -> 'b) T.typ ->
		('a -> 'b) fptr' * 'a -> 'b
end

(*
 * iptr.sig   Interface to an "incomplete" C pointer type.
 *
 *   The code generated from a C interface (call it <A>) containing an
 *   "incomplete" type (i.e., a pointer to a named but otherwise undefined
 *   struct or union, call it <foo>) will be a functor that takes a structure
 *   argument representing this incomplete (pointer-)type.  The signature
 *   for this argument structure is POINTER_TO_INCOMPLETE_TYPE.
 *
 *   Therefore, for each incomplete struct <foo> there will be a functor
 *   argument
 *        structure I_S_<foo> : POINTER_TO_INCOMPLETE_TYPE
 *   and for each incomplete union <bar> there will be a functor argument
 *        structure I_U_<bar> : POINTER_TO_INCOMPLETE_TYPE
 *   in the definition of functor <A>.<A>Fn.
 *
 * Actual structures matching POINTER_TO_INCOMPLETE_TYPE can be generated
 * using the functors PointerToCompleteType (see c-iptr-fn.sml) and
 * PointerToIncompleteType (see i-iptr-fn.sml).
 *
 *   (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature POINTER_TO_INCOMPLETE_TYPE = sig
    type 'c iptr			(* = (? su, unit, 'c) ptr  *)
    type 'c iptr'			(* = (? su, unit, 'c) ptr' *)

    val typ'rw : (C.rw iptr, unit) C.T.typ
    val typ'ro : (C.ro iptr, unit) C.T.typ

    val light : 'c iptr -> 'c iptr'
    val heavy : 'c iptr' -> 'c iptr

    val get : ('pc iptr, unit, 'c) C.obj -> 'pc iptr
    val get' : ('pc iptr, unit, 'c) C.obj' -> 'pc iptr'

    val set : ('pc iptr, unit, C.rw) C.obj * 'pc iptr -> unit
    val set' : ('pc iptr, unit, C.rw) C.obj' * 'pc iptr' -> unit

    val set_voidptr : ('pc iptr, unit, C.rw) C.obj * C.voidptr -> unit
    val set_voidptr' : ('pc iptr, unit, C.rw) C.obj' * C.voidptr -> unit

    val compare : 'c iptr * 'c iptr -> order
    val compare' : 'c iptr' * 'c iptr' -> order

    val inject : 'c iptr -> C.voidptr
    val inject' : 'c iptr' -> C.voidptr

    val project : ('c iptr, unit) C.T.typ -> C.voidptr -> 'c iptr
    val project' : ('c iptr, unit) C.T.typ -> C.voidptr -> 'c iptr'

    val null : ('c iptr, unit) C.T.typ -> 'c iptr
    val null' : 'c iptr'
end

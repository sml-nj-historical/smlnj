(*
 * c-iptr-fn.sml   Generate the "completion" of an "incomplete" C type.
 *
 *   The code generated from a C interface (call it <A>) containing an
 *   "incomplete" type (i.e., a pointer to a named but otherwise undefined
 *   struct or union, call it <foo>) will be a functor that takes a structure
 *   argument representing this incomplete (pointer-)type.
 *
 *   If (struct or union) <foo> has a known definition in some other
 *   participating C interface (call it <B>), then one must
 *   propagate the definition from <B> trough <A> to clients of <A> as
 *   follows:
 * 
 *   <A>.<A>Fn (...
 *              structure I_S_<foo> = PointerToCompleteType (<B>.S_<foo>)
 *              ...)
 *
 *   If no definition for <foo> is available, then one must use a brand-new
 *   I_S_<foo>.  See i-iptr-fn.sml.
 *
 *   (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
functor PointerToCompleteType (type tag val typ : tag C.su C.T.typ) :>
	POINTER_TO_INCOMPLETE_TYPE
	where type 'c iptr = (tag C.su, 'c) C.ptr
        where type 'c iptr' = (tag C.su, 'c) C.ptr' =
struct
    type 'c iptr = (tag C.su, 'c) C.ptr
    type 'c iptr' = (tag C.su, 'c) C.ptr'

    val typ'rw = C.T.pointer typ
    val typ'ro = C.T.ro typ'rw

    val light = C.Light.ptr
    fun heavy p = C.Heavy.ptr typ p

    val get = C.Get.ptr
    val get' = C.Get.ptr'

    val set = C.Set.ptr
    val set' = C.Set.ptr'

    val set_voidptr = C.Set.ptr_voidptr
    val set_voidptr' = C.Set.ptr_voidptr'

    val compare = C.Ptr.compare
    val compare' = C.Ptr.compare'

    val inject = C.Ptr.inject
    val inject' = C.Ptr.inject'

    val cast = C.Ptr.cast
    val cast' = C.Ptr.cast'

    val null = C.Ptr.null
    val null' = C.Ptr.null'
end

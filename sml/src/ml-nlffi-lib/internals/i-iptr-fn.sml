(*
 * i-iptr-fn.sml   Leave an "incomplete" C type "incomplete".
 *
 *   The code generated from a C interface (call it <A>) containing an
 *   "incomplete" type (i.e., a pointer to a named but otherwise undefined
 *   struct or union, call it <foo>) will be a functor that takes a structure
 *   argument representing this incomplete (pointer-)type.
 *
 *   If (struct or union) <foo> has no known definition in any other
 *   participating C interface (i.e., when it is abstract), then
 *   one must generate a new structure (containing brand-new, abstract types)
 *   to be supplied as the functor's argument:
 * 
 *   <A>.<A>Fn (...
 *          structure I_S_<foo> = PointerToIncompleteType ()
 *          ...)
 *
 *   If a definition for <foo> is available, then one must use that
 *   instead. See c-iptr-fn.sml.
 *
 *   If the same abstract type is being used abstractly in several
 *   different participating C interfaces, then one must pass the same
 *   structure to all relevant functors:
 *
 *   structure I_S_<foo> = PointerToIncompleteType ()
 *   <A>.<A>Fn (... structure I_I_<foo> = I_S_<foo> ...)
 *   <B>.<B>Fn (... structure I_I_<foo> = I_S_<foo> ...)
 *   ...
 *
 *   (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
functor PointerToIncompleteType () :> POINTER_TO_INCOMPLETE_TYPE = struct

    type 'c iptr = (C.uchar, unit, 'c) C.ptr
    type 'c iptr' = (C.uchar, unit, 'c) C.ptr'

    val typ'rw = C.T.pointer C.T.uchar
    val typ'ro = C.T.ro typ'rw

    val light = C.Light.ptr
    fun heavy p = C.Heavy.ptr C.T.uchar p

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

    val project = C.Ptr.project
    val project' = C.Ptr.project'

    val null = C.Ptr.null
    val null' = C.Ptr.null'
end

(*
 * c-iptr-fn.sml   Generate the "completion" of an "incomplete" C type.
 *
 *   See ../iptr.sig for details.
 *
 *   (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
functor PointerToCompleteType (type tag val typ : tag C.su C.T.typ) :>
	POINTER_TO_INCOMPLETE_TYPE
	where type 'c iobj = (tag C.su, 'c) C.obj =
struct
    type 'c iobj = (tag C.su, 'c) C.obj

    val typ'rw = C.T.pointer typ
    val typ'ro = C.T.ro typ'rw
end

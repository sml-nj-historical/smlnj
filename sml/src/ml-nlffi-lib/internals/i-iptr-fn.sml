(*
 * i-iptr-fn.sml   Leave an "incomplete" C type "incomplete".
 *
 *   See ../iptr.sig for details.
 *
 *   (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
functor PointerToIncompleteType () :> POINTER_TO_INCOMPLETE_TYPE = struct

    type 'c iobj = (C.uchar, 'c) C.obj

    val typ'rw = C.T.pointer C.T.uchar
    val typ'ro = C.T.ro typ'rw
end

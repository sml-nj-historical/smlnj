(*
 * iptr.sig   Interface to an "incomplete" C pointer type.
 *
 *     The idea is that 'c iobj stands for some (T su, 'c) obj.
 *     However, since the struct/union in question has not yet been
 *     declared, T is not yet known.  C does let you have pointers
 *     to such "incomplete" types, and so do we:  It is ok to form
 *     a 'c iobj ptr.  Many of the usual pointer operations are
 *     possible, but there are some (subscript, pointer
 *     arithmetic, dereferencing) that are not.
 *     The C signature (in c.sig) expresses this by explicitly
 *     contraining, e.g., the dereference operator |*| to an argument
 *     of type ('t, 'c) obj ptr instead of the more general 'o ptr.
 *
 *   For each incomplete struct (union) <foo>, the ml-nlffigen tool will
 *   generate code that refers to the ML type 'c IS_<foo>.iobj
 *   ('c IU_<foo>iobj) where structure IS_<foo> (IU_<foo>) matches
 *   signature POINTER_TO_INCOMPLETE_TYPE.
 *
 *   By default, the same tool will also generate definitions for these
 *   ML structures, making each of the iobj types abstract (by invoking
 *   functor PointerToIncompleteType from internals/i-iptr-fn.sml).
 *
 *   If the complete definition of the corresponding S_<foo> (U_<foo>) is
 *   available, one can alternatively define IS_<foo> (IU_<foo>) by invoking
 *   functor PointerToCompleteType (internals/c-iptr-fn.sml).  This
 *   functor reveals (and makes concrete) the conceptual type identity of
 *
 *      (S_<foo>.tag su, 'c) obj == 'c IS_<foo>.iobj
 *   or
 *      (U_<foo>.tag su, 'c) obj == 'c IU_<foo>.iobj
 *
 *   Use the "-incomplete" command line option of ml-nlffigen to suppress
 *   automatic generation of definitions for abstract I{S|U}_<foo>.iobj types.
 * 
 *   If you do want to utilize automatic generation of such types but
 *   want to share them across outputs of multiple invocations of
 *   ml-nlffigen, use the "-iptr" command line option of ml-nlffigen.
 *   (If you do not share these definitions in ML, fresh types well
 *   up being generated for every C interface -- potentially rendering
 *   the results incompatible with each other.)
 *
 *   (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature POINTER_TO_INCOMPLETE_TYPE = sig
    type 'c iobj

    (* RTTI for the _pointer_ types: *)
    val typ'rw : C.rw iobj C.ptr C.T.typ
    val typ'ro : C.ro iobj C.ptr C.T.typ
end

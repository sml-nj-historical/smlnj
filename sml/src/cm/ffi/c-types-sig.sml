(*
 * Encoding the C type system in SML.
 *   Most of the type system is encoded here, the only things missing
 *   are "struct" and "union".  Those are "generative" and each such
 *   type must be simulated using a fresh ML type.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CTYPES = sig

    (* C objects and their types *)
    type 't objtype			(* "first-class" C object types *)
    type 't obj

    (* The main purpose of having objtype is to be able to compute sizes. *)
    val sizeof : 't objtype -> word

    (* Things that can be substituted for 't... *)

    (* We must keep all things opaque to make sure they are treated
     * as distinct types by ML ... *)
    type c_char
    type c_uchar
    type c_int
    type c_uint
    type c_short
    type c_ushort
    type c_long
    type c_ulong
    type c_float
    type c_double

    (* Corresponding "revealed" types. *)
    type ml_char = CMemory.schar
    type ml_uchar = CMemory.uchar
    type ml_int = CMemory.sint
    type ml_uint = CMemory.uint
    type ml_short = CMemory.sshort
    type ml_ushort = CMemory.ushort
    type ml_long = CMemory.slong
    type ml_ulong = CMemory.ulong
    type ml_float = CMemory.float
    type ml_double = CMemory.double

    (* if "'t" is a C type, then so is "'t ptr" *)
    type 't ptr

    (* C's "void *"...
     *   We make this into a separate type because dereferencing
     *   through a void * is not valid anyway.  In other words,
     *   in our world there is no "void". *)
    type c_voidptr

    (* An array of length "('n, Dim.nonzero) Dim.dim" with elements of
     *  type "'t" has type "('t, 'n) arr"... *)
    type ('t, 'n) arr

    (* C functions are represented by function pointers here.  In a way
     * similar to void*, we treat these function pointers as primitive
     * types.  To put it differently: There are no "function objects".
     * To distinguish between different function calling protocols,
     * function pointers are annotated by the function's type signature 's. *)
    type 's fptr

    (* The type of a C struct or union...
     *   For each distinct struct- or union-type there must be a unique
     *   "tag" type 'tag. *)
    type 'tag su

    (* primitive objtypes *)
    val t_char : c_char objtype
    val t_uchar : c_uchar objtype
    val t_int : c_int objtype
    val t_uint : c_uint objtype
    val t_short : c_short objtype
    val t_ushort : c_ushort objtype
    val t_long : c_long objtype
    val t_ulong : c_ulong objtype
    val t_float : c_float objtype
    val t_double : c_double objtype

    (* according to what we said above, void* is a primitive type here *)
    val t_voidptr : c_voidptr objtype

    (* getters for certain objects ... *)
    val get_char : c_char obj -> ml_char
    val get_uchar : c_uchar obj -> ml_uchar
    val get_int : c_int obj -> ml_int
    val get_uint : c_uint obj -> ml_uint
    val get_short : c_short obj -> ml_short
    val get_ushort : c_ushort obj -> ml_ushort
    val get_long : c_long obj -> ml_long
    val get_ulong : c_ulong obj -> ml_ulong
    val get_float : c_float obj -> ml_float
    val get_double : c_double obj -> ml_double

    val get_ptr : 't ptr obj -> 't ptr
    val get_fptr : 's fptr obj -> 's fptr
    val get_voidptr : c_voidptr obj -> c_voidptr

    (* setters for those objects ... *)
    val set_char : c_char obj * ml_char -> unit
    val set_uchar : c_uchar obj * ml_uchar -> unit
    val set_int : c_int obj * ml_int -> unit
    val set_uint : c_uint obj * ml_uint -> unit
    val set_short : c_short obj * ml_short -> unit
    val set_ushort : c_ushort obj * ml_ushort -> unit
    val set_long : c_long obj * ml_long -> unit
    val set_ulong : c_ulong obj * ml_ulong -> unit
    val set_float : c_float obj * ml_float -> unit
    val set_double : c_double obj * ml_double -> unit

    val set_ptr : 't ptr obj * 't ptr -> unit
    val set_fptr : 's fptr obj * 's fptr -> unit
    val set_voidptr : c_voidptr obj * c_voidptr -> unit

    (* "&" and "*" -- both for objects and on corresponding types *)
    val ptr : 't obj -> 't ptr
    val ptr_type : 't objtype -> 't ptr objtype
    val target : 't ptr -> 't obj
    val target_type : 't ptr objtype -> 't objtype

    (* pointer casts (those that are permitted even by strict C) *)
    val ptr_inject : 't ptr -> c_voidptr
    val ptr_project : 't objtype -> c_voidptr -> 't ptr

    (* construct ('t, 'n) arr objtype *)
    val arr_type :
	't objtype * ('n, Dim.nonzero) Dim.dim -> ('t, 'n) arr objtype
    val elem_type : ('t, 'n) arr objtype -> 't objtype

    (* array subscript (unchecked!) *)
    val arr_sub : 't objtype -> ('t, 'n) arr obj * int -> 't obj

    (* array subscipt (checked) *)
    val arr_safe_sub : ('t, 'n) arr objtype -> ('t, 'n) arr obj * int -> 't obj

    (* decay array object to pointer *)
    val arr_decay : ('t, 'n) arr obj -> 't ptr

    (* other pointer operations *)
    val null : 't ptr
    val isNull : 't ptr -> bool

    (* pointer arithmetic (need base type for that) *)
    val padd : 't objtype -> 't ptr * int -> 't ptr

    (* pointer "subscript" -- a combination of "target" and "padd" *)
    val ptr_sub : 't objtype -> 't ptr * int -> 't obj

    (* "bitfield" encapsulates the struct's address and the access
     * coordinates of the bitfield. *)
    type bitfield

    val getBitfield : bitfield -> ml_uint
    val setBitfield : bitfield * ml_uint -> unit
end

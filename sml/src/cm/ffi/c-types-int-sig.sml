(*
 * A "private" extension to the encoding of C types in SML.
 *   The routines here are for use by code that will be automatically
 *   generated from corresponding C files.  User code is not supposed
 *   to access them because they are unsafe.  (As if subverting the C
 *   type system were such a big deal...)
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CTYPES_INTERNAL = sig
    include CTYPES

    type addr

    (* --- generic --- *)

    (* generate objects from adresses *)
    val mkObj : 't objtype -> addr -> 't obj

    (* --- struct and union --- *)

    (* Generate the access method for member of type 'm within
     * a struct of type 's at a given offset.
     * This is an unsafe operation only to be used by automatically
     * generated code.  (It must correspond with the actual C source.)
     * Objtypes are given to avoid the need for explicit ML "casts"
     * at this point.
     * The offset is in bytes. *)
    val mkStructMember :
	{ stype: 's su objtype, mtype: 'm objtype, offset: int } ->
	's su obj -> 'm obj

    (* Same for union.  Here we don't need an explicit offset. *)
    val mkUnionMember :
	{ utype: 'u su objtype, mtype: 'm objtype } ->
	'u su obj -> 'm obj

    val mkBitfield :
	{ stype: 's su objtype, offset: int, mask: ml_uint, shift: word } ->
	's su obj -> bitfield

    (* Generate a new struct- or union-objtype, given its size... *)
    val mkSUType : 'tag -> word -> 'tag su objtype

    (* --- function pointer --- *)

    (* Make a new function pointer objtype; an (unused) function of type 's
     * should be passed in as a way of specifying the type 's.
     * (Another method would be to pass a unit object, requiring an
     *  explicit type constraint or something like that.  I consider
     *  the latter somewhat error-prone.) *)
    val mkFptrType : 's -> 's fptr objtype

    (* Since we said that functions aren't objects in our world, we
     * need a separate interface to generate fptr values.  Moreover,
     * we also need the opposite -- revealing the address that represents
     * function pointer... *)
    val revealFptr : 's fptr -> addr
    val mkFptr : 's fptr objtype -> addr -> 's fptr
end

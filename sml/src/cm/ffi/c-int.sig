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
signature C_INT = sig
    include C

    type addr = CMemory.addr

    val mk_ptr :    (('t, 'f, 'c) ptr, 'f) T.typ -> addr -> ('t, 'f, 'c) ptr
    val mk_voidptr : addr -> voidptr
    val mk_fptr : 'f T.fptr_typ -> addr -> 'f fptr

    val mk_rw_acc : ('m, 'f) T.typ -> int -> ('s, 'm, 'f) rw_acc
    val mk_ro_acc : ('m, 'f) T.typ -> int -> ('s, 'm, 'f) ro_acc

    val mk_bf_rw_acc :
	{ offset: int, mask: ml_uint, shift: word } -> 's bf_rw_acc
    val mk_bf_ro_acc :
	{ offset: int, mask: ml_uint, shift: word } -> 's bf_ro_acc

    val mk_su_typ : word -> 's T.su_typ
    val mk_fptr_typ : (addr -> 'a -> 'b) -> ('a -> 'b) T.fptr_typ
end

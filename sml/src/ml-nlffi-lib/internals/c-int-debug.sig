(*
 * A "private" extension to the encoding of C types in SML.
 *   The routines here are for use by code that will be automatically
 *   generated from corresponding C files.  User code is not supposed
 *   to access them because they are unsafe.  (As if subverting the C
 *   type system were such a big deal...)
 *
 * DEBUG VERSION with CHECKED POINTER DEREFERENCING.
 * 
 *   (C) 2002, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature C_INT_DEBUG = sig
    exception NullPointer
    include C_INT
end

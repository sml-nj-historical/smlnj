(* bug1248.1.sml *)

val blastWrite : 'a -> Word8Vector.vector = 
    (fn x => Unsafe.CInterface.c_function "SMLNJ-RunT" "blastOut" x);
blastWrite (Word8Array.array (516000, 0w0));
blastWrite (Word8Array.array (516001, 0w0));

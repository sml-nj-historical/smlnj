(* bug635.sml *)
(* byteArray.update and ByteArray.sub raise Ord *)

val a = Word8Array.array(2, 0w0);
Word8Array.update (a, 2, 0w100) handle e => print ((exnName e)^"\n");
Word8Array.sub(a,2);


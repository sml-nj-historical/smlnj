(* bug1012.sml *)

val (low, high) = (Word32.fromInt 0xde32, Word32.fromInt 0x8002);
val high = 0w32770 : Word32.word
val low = 0w56882 : Word32.word
val x = Word32.orb (Word32.<< (high, 0w16), low);
Word32.toInt x handle e => (print (exnName e ^ "\n"); 0);
Word32.toIntX x handle e => (print (exnName e ^ "\n"); 0);

(* bug714.sml *)
(* 714. failure of ByteArray.update *)


val a = Word8Array.array(5, 0w65); 
Word8Array.foldr (op ::) nil a;
Word8Array.update (a, 3, 0w66);
Word8Array.foldr (op ::) nil a;
Word8Array.update (a, 0, 0w67);
Word8Array.foldr (op ::) nil a;


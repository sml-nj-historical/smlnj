(* bug1034.sml *)
(* this should produce a type error *)

String.size(Word8Vector.fromList []);

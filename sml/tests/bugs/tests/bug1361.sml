(* bug1361.sml *)
(* equality returns false on alpha *)

val i = 0wx7fffffff : Word32.word;
i = 0wx7fffffff;

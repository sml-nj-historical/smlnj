(* bug1143.sml *)

val maskr = ref 0wx3fff;
Word.andb (!maskr, 0wxfffffff);
Word.toInt (Word.andb (!maskr, 0wxfffffff));

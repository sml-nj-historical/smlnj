(* bug247.sml *)
(* close_out std_out yields uncaught exception *)

TextIO.closeOut TextIO.stdOut;

(* bug949.3.sml *)

datatype num = zero | one_more_than of num;
one_more_than(0);

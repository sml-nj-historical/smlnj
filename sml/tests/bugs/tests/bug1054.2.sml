(* bug1054.2.sml *)

datatype t1 = c1 of t2 * t3
withtype t2 = t1 list
     and t3 = t2 list;

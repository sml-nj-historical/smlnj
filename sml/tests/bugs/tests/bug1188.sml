(* bug1188.sml *)

datatype K = A of int | B of {one:int, two:string};
val b = B {one = 1, two="TWO"};
#two b;

datatype 'a T = E | B of int * 'a T;

val p = B (0, B (2, B (4, E)));

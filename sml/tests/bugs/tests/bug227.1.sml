(* bug227.1.sml *)

(* equality on datatypes *)

datatype 'a d1 = c11 | c12 of ('a * 'a d1);
datatype 'a d2 = c21 | c22 of ('a d2 * 'a );
datatype    d3 = c31 | c32 of ( d3 * int);

c11 = c11; (* works correctly *)
c12(1,c11) = c12(1,c11);  (* 0.56: uncaught exception Match; 0.59: ok *)
c21 = c21; (* loops forever in 0.56 and 0.59 *)
c22(c21,1) = c22(c21,1);  (* 0.56: uncaught exception Match; 0.59: ok *)
c31 = c31; (* works correctly *)
c32(c31,1) = c32(c31,1);  (* 0.56: uncaught exception Match; 0.59: ok *)



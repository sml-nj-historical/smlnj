(* bug570.sml *)
(* type inference for flexrecords re: equality types *)

fun force_record {x=x, y=y} = 3;

fun force_eq x = (x = x);

fun bar (r as {x=x, ...}) = (force_record r; force_eq r);

fun foo (r as {x=x, ...}) = (force_eq r; force_record r);

(* The last line should not return an equality type required error *)

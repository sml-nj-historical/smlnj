(* bug1433.1.sml *)

structure A:>sig type t eqtype u=t val v:u end=
struct type t=int type u=int val v=1 end;
(A.v=A.v);

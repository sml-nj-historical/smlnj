(* bug1311.sml *)

structure A =
struct
  datatype t = C of ('a * 'a)
  fun f (cons, C(x,y)) = C(cons x, cons y)
end;

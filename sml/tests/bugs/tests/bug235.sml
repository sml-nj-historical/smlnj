(* bug235.sml *)

signature SIG1 =
sig
  eqtype ('a,'a) t1
  type ('a,'a) t2
end;

signature SIG2 =
sig
  datatype ('a,'a) t3 = foo of 'a
end;

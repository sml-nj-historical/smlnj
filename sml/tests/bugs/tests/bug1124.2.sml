(* bug1124.2.sml *)

signature SIG =
sig
  datatype t1 = C1
  datatype t2 = C2
  and t3 = C3 | C4 of t1
end;

functor F(structure S : SIG) =
struct
  fun f(S.C4 _) = true
    | f x = x=x
end;

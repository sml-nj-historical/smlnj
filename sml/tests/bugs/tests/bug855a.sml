(* bug855a.sml *)
(* bad error message *)

signature C1 = sig type R val foo : R -> R end;
funsig Soo(structure T1:C1 structure T2:sig include C1 end) =
  sig include C1 sharing T1 = T2 end;

functor Foo(structure T1:C1 structure T2:sig include C1 end):
        sig include C1 sharing T1 = T2 end =
struct type R = T2.R val foo = T1.foo end;  

(* std_in:6.33-6.39 Error: definitional sharing constraint T1 = T2 can never be satisfied
*)

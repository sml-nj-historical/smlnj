(* bug 331 -- should give the following message:
   std_in:2.13-2.40 Error: explicit type variable cannot be generalized at its scoping declaration:
   'a
*)
      fn x => let val y : 'a -> 'a = fn z => x in y end;

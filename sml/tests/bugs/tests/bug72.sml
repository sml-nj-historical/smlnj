(* bug72.sml
72. equality types with abstype declarations
*)

abstype 'a foo = Foo of 'a list
with fun test(Foo x) = (x = []) end;

abstype 'a foo = Foo of 'a list
with fun test(x as Foo _) = (x = Foo []) end;


(* bug1002.sml *)

abstype 'a foo = Foo of 'a list
with
  fun test x = (x = Foo [])
end;

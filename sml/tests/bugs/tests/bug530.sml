(* bug530.sml *)
(* missing space in printing abstype declaration *)

abstype 'a foo = FOO of 'a list
with
  fun mkfoo() = FOO []
end;

(* should not print out type'a but rather type 'a *)

(* bug231.sml *)
(* A type abbreviation for an abstract type admits equality. *)

abstype A = A
with
  type B = A
end;

fn (x: B) => (x=x);  (* should generate an error *)
